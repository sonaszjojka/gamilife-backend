package edu.pjwstk.auth.controllers;

import edu.pjwstk.auth.dto.request.LoginUserRequest;
import edu.pjwstk.auth.dto.request.RegisterUserRequest;
import edu.pjwstk.auth.dto.response.AccessTokenResponse;
import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.auth.services.AuthService;
import edu.pjwstk.auth.util.CookieUtil;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityRequirements;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@SecurityRequirements
@RestController
@RequestMapping("/api/v1/auth")
public class AuthController {

    private final AuthService authService;
    private final CookieUtil cookieUtil;

    public AuthController(AuthService authService, CookieUtil cookieUtil) {
        this.authService = authService;
        this.cookieUtil = cookieUtil;
    }

    @PostMapping("/register")
    public ResponseEntity<Void> registerUser(@RequestBody @Valid RegisterUserRequest request,
                                             HttpServletResponse response) {
        BasicUserInfoApiDto user = authService.registerUser(
                new RegisterUserDto(
                        request.firstName(),
                        request.lastName(),
                        request.email(),
                        request.password(),
                        request.username(),
                        request.dateOfBirth(),
                        request.sendBudgetReports(),
                        request.isProfilePublic()
                )
        );
        AuthTokens authTokens = authService.issueJwtTokens(user);

        Cookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());
        Cookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());

        response.addCookie(refreshTokenCookie);
        response.addCookie(accessTokenCookie);

        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @PostMapping("/login")
    public ResponseEntity<AccessTokenResponse> loginUser(@RequestBody @Valid LoginUserRequest request,
                                                         HttpServletResponse response) {
        AuthTokens authTokens = authService.loginUser(
                new LoginUserDto(
                        request.email(),
                        request.password()
                )
        );

        Cookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());
        Cookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());

        response.addCookie(refreshTokenCookie);
        response.addCookie(accessTokenCookie);

        return ResponseEntity.ok(new AccessTokenResponse(authTokens.accessToken()));
    }

    @PostMapping("/logout")
    public ResponseEntity<Void> logout(@CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        authService.logoutUser(refreshToken);

        response.addCookie(cookieUtil.invalidateAccessTokenCookie());
        response.addCookie(cookieUtil.invalidateRefreshTokenCookie());

        return ResponseEntity.ok().build();
    }

    @SecurityRequirement(name = "refreshToken")
    @PostMapping("/refresh")
    public ResponseEntity<Void> refreshTokens(
            @CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        AuthTokens authTokens = authService.refreshAccessToken(refreshToken);

        Cookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
        response.addCookie(accessTokenCookie);

        return ResponseEntity.ok().build();
    }
}
