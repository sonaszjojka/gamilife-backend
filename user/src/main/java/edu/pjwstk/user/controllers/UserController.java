package edu.pjwstk.user.controllers;

import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.commonweb.CookieUtil;
import edu.pjwstk.user.dto.request.ChangeUserPasswordRequest;
import edu.pjwstk.user.dto.response.CurrentUserInfoResponse;
import edu.pjwstk.user.dto.service.ChangeUserPasswordCommand;
import edu.pjwstk.user.usecase.ChangeUserPasswordUseCase;
import edu.pjwstk.user.usecase.GetUserByIdUseCase;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@SecurityRequirement(name = "accessToken")
@RestController
@RequestMapping("/api/v1/users")
@AllArgsConstructor
public class UserController {

    private GetUserByIdUseCase getUserByIdUseCase;
    private ChangeUserPasswordUseCase changeUserPasswordUseCase;
    private CookieUtil cookieUtil;

    @GetMapping("/{userId}")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<CurrentUserInfoResponse> getCurrentUserInfo(@PathVariable UUID userId) {
        BasicUserInfoApiDto dto = getUserByIdUseCase.execute(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return ResponseEntity.ok(new CurrentUserInfoResponse(
                dto.email()
        ));
    }

    @PatchMapping("/{userId}/password")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<CurrentUserInfoResponse> changePassword(
            @PathVariable UUID userId,
            @RequestBody ChangeUserPasswordRequest request,
            HttpServletResponse response) {
        AuthTokens tokens = changeUserPasswordUseCase.execute(new ChangeUserPasswordCommand(
                userId, request.oldPassword(), request.newPassword()
        ));

        ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(tokens.accessToken());
        ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(tokens.refreshToken());

        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

        return ResponseEntity.noContent().build();
    }
}
