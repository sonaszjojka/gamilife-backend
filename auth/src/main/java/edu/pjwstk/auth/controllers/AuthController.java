package edu.pjwstk.auth.controllers;

import edu.pjwstk.auth.dto.request.*;
import edu.pjwstk.auth.dto.response.AfterLoginResponse;
import edu.pjwstk.auth.dto.service.*;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.usecase.*;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.commonweb.CookieUtil;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityRequirements;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@SecurityRequirements
@RestController
@RequestMapping("/api/v1/auth")
@AllArgsConstructor
public class AuthController {

    private final RegisterUserUseCase registerUserUseCase;
    private final LoginUserUseCase loginUserUseCase;
    private final LogoutUserUseCase logoutUserUseCase;
    private final RefreshAccessTokenUseCase refreshAccessTokenUseCase;
    private final SendEmailVerificationCodeUseCase sendEmailVerificationCodeUseCase;
    private final GetAuthenticatedUserDataUseCase getAuthenticatedUserDataUseCase;
    private final VerifyEmailUseCase verifyEmailUseCase;
    private final GenerateAndSendForgotPasswordTokenUseCase generateAndSendForgotPasswordTokenUseCase;
    private final ResetPasswordUseCase resetPasswordUseCase;
    private final CookieUtil cookieUtil;

    @PostMapping("/register")
    public ResponseEntity<Void> registerUser(@RequestBody @Valid RegisterUserRequest request) {
        registerUserUseCase.execute(
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

        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @PostMapping("/login")
    public ResponseEntity<AfterLoginResponse> loginUser(@RequestBody @Valid LoginUserRequest request,
                                          HttpServletResponse response) {
        LoginUserResult result = loginUserUseCase.execute(
                new LoginUserDto(
                        request.email(),
                        request.password()
                )
        );

        AuthTokens authTokens = result.authTokens();

        ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
        ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());

        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

        return ResponseEntity.ok(AfterLoginResponse.from(result));
    }

    @PreAuthorize("hasAnyRole('VERIFIED', 'UNVERIFIED')")
    @PostMapping("/logout")
    public ResponseEntity<Void> logout(@CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        logoutUserUseCase.execute(refreshToken);

        ResponseCookie accessTokenCookie = cookieUtil.invalidateAccessTokenCookie();
        ResponseCookie refreshTokenCookie = cookieUtil.invalidateRefreshTokenCookie();

        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

        return ResponseEntity.ok().build();
    }

    @PreAuthorize("hasAnyRole('VERIFIED', 'UNVERIFIED')")
    @SecurityRequirement(name = "refreshToken")
    @PostMapping("/refresh")
    public ResponseEntity<Void> refreshTokens(
            @CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        AuthTokens authTokens = refreshAccessTokenUseCase.execute(refreshToken);

        ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());

        return ResponseEntity.ok().build();
    }

    @PreAuthorize("hasRole('UNVERIFIED')")
    @SecurityRequirement(name = "accessToken")
    @PostMapping("/email-verifications/confirm")
    public ResponseEntity<AfterLoginResponse> verifyEmail(@RequestBody @Valid EmailVerificationCodeRequest emailVerificationCodeRequest,
                                                          HttpServletResponse response) {
        CurrentUserDto user = getAuthenticatedUserDataUseCase.execute()
                .orElseThrow(() -> new InvalidCredentialsException("Provided access token is invalid"));

        LoginUserResult result = verifyEmailUseCase.execute(new EmailVerificationCode(
                user.userId(), emailVerificationCodeRequest.code()
        ));

        AuthTokens authTokens = result.authTokens();

        ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
        ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());

        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

        return ResponseEntity.ok(AfterLoginResponse.from(result));
    }

    @PreAuthorize("hasRole('UNVERIFIED')")
    @SecurityRequirement(name = "accessToken")
    @PostMapping("/email-verifications/resend")
    public ResponseEntity<Void> resendVerificationCode() {
        CurrentUserDto user = getAuthenticatedUserDataUseCase.execute()
                .orElseThrow(() -> new InvalidCredentialsException("Provided access token is invalid"));

        sendEmailVerificationCodeUseCase.execute(user.userId());

        return ResponseEntity.accepted().build();
    }

    @PostMapping("/forgot-password")
    public ResponseEntity<Void> forgotPassword(@Valid @RequestBody ForgotPasswordRequest request) {
        generateAndSendForgotPasswordTokenUseCase.execute(request.email());

        return ResponseEntity.noContent().build();
    }

    @PostMapping("/reset-password")
    public ResponseEntity<Void> resetPassword(@Valid @RequestBody ResetPasswordRequest request) {
        resetPasswordUseCase.execute(new ResetPasswordCommand(
                request.code(), request.newPassword()
        ));

        return ResponseEntity.noContent().build();
    }
}
