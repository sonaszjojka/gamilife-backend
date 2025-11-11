package edu.pjwstk.user.controllers;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.groups.GroupApi;
import edu.pjwstk.api.groups.dto.FindAllGroupsByUserIdWhereUserIsMemberResult;
import edu.pjwstk.commonweb.CookieUtil;
import edu.pjwstk.user.dto.request.ChangeUserPasswordRequest;
import edu.pjwstk.user.dto.response.CurrentUserInfoResponse;
import edu.pjwstk.user.dto.response.UserDetailsResponse;
import edu.pjwstk.user.dto.service.ChangeUserPasswordCommand;
import edu.pjwstk.user.dto.service.UserDetailsDto;
import edu.pjwstk.user.usecase.ChangeUserPasswordUseCase;
import edu.pjwstk.user.usecase.GetUserDetailsUseCase;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
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

    private GetUserDetailsUseCase getUserDetailsUseCase;
    private ChangeUserPasswordUseCase changeUserPasswordUseCase;
    private CookieUtil cookieUtil;
    private GroupApi groupsApi;

    @GetMapping("/{userId}")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<UserDetailsResponse> getCurrentUserDetails(@PathVariable UUID userId) {
        UserDetailsDto dto = getUserDetailsUseCase.execute(userId);

        return ResponseEntity.ok(UserDetailsResponse.from(dto));
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

    @GetMapping("/{userId}/groups")
    @PreAuthorize("@userSecurity.matchesTokenUserId(authentication, #userId)")
    public ResponseEntity<FindAllGroupsByUserIdWhereUserIsMemberResult> getAllGroupsByUserId(
            @PathVariable("userId") UUID userId,

            @RequestParam(required = false) String joinCode,

            @RequestParam(required = false) Integer groupType,

            @RequestParam(required = false) String groupName,

            @RequestParam(defaultValue = "0")
            @Min(0) Integer page,

            @RequestParam(defaultValue = "10")
            @Min(1) @Max(100) Integer size

    ){
        FindAllGroupsByUserIdWhereUserIsMemberResult response =  groupsApi
                .findAllGroupsByUserIdWhereUserIsMember(userId, page, size, joinCode, groupType, groupName);
        return ResponseEntity.ok(response);
    }
}
