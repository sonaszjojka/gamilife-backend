package pl.gamilife.auth.controller;

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
import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.auth.controller.request.*;
import pl.gamilife.auth.controller.response.AfterLoginResponse;
import pl.gamilife.auth.usecase.common.LoginUserResult;
import pl.gamilife.auth.usecase.getauthuser.GetAuthenticatedUserCommand;
import pl.gamilife.auth.usecase.getauthuser.GetAuthenticatedUserDataUseCase;
import pl.gamilife.auth.usecase.login.LoginUserCommand;
import pl.gamilife.auth.usecase.login.LoginUserUseCase;
import pl.gamilife.auth.usecase.logout.LogoutUserCommand;
import pl.gamilife.auth.usecase.logout.LogoutUserUseCase;
import pl.gamilife.auth.usecase.refreshtoken.RefreshAccessTokenCommand;
import pl.gamilife.auth.usecase.refreshtoken.RefreshAccessTokenUseCase;
import pl.gamilife.auth.usecase.registeruser.RegisterUserCommand;
import pl.gamilife.auth.usecase.registeruser.RegisterUserUseCase;
import pl.gamilife.auth.usecase.resendemailverification.ResendEmailVerificationCodeCommand;
import pl.gamilife.auth.usecase.resendemailverification.ResendEmailVerificationCodeUseCase;
import pl.gamilife.auth.usecase.resetpassword.ResetPasswordCommand;
import pl.gamilife.auth.usecase.resetpassword.ResetPasswordUseCase;
import pl.gamilife.auth.usecase.sendforgotpasswordcode.SendForgotPasswordCodeCommand;
import pl.gamilife.auth.usecase.sendforgotpasswordcode.SendForgotPasswordTokenUseCase;
import pl.gamilife.auth.usecase.verifyemail.VerifyEmailCommand;
import pl.gamilife.auth.usecase.verifyemail.VerifyEmailUseCase;
import pl.gamilife.infrastructure.web.util.CookieUtil;

@SecurityRequirements
@RestController
@RequestMapping("/api/v1/auth")
@AllArgsConstructor
public class AuthController {

    private final RegisterUserUseCase registerUserUseCase;
    private final LoginUserUseCase loginUserUseCase;
    private final LogoutUserUseCase logoutUserUseCase;
    private final RefreshAccessTokenUseCase refreshAccessTokenUseCase;
    private final ResendEmailVerificationCodeUseCase resendEmailVerificationCodeUseCase;
    private final GetAuthenticatedUserDataUseCase getAuthenticatedUserDataUseCase;
    private final VerifyEmailUseCase verifyEmailUseCase;
    private final SendForgotPasswordTokenUseCase sendForgotPasswordTokenUseCase;
    private final ResetPasswordUseCase resetPasswordUseCase;
    private final CookieUtil cookieUtil;

    @PostMapping("/register")
    public ResponseEntity<Void> registerUser(@RequestBody @Valid RegisterUserRequest request) {
        registerUserUseCase.execute(
                new RegisterUserCommand(
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
                new LoginUserCommand(
                        request.email(),
                        request.password()
                )
        );

        setTokenCookies(result.authTokens(), response);

        return ResponseEntity.ok(AfterLoginResponse.from(result));
    }

    @PostMapping("/logout")
    public ResponseEntity<Void> logout(@CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        logoutUserUseCase.execute(new LogoutUserCommand(refreshToken));

        invalidateTokenCookies(response);

        return ResponseEntity.ok().build();
    }

    @SecurityRequirement(name = "refreshToken")
    @PostMapping("/refresh")
    public ResponseEntity<Void> refreshTokens(
            @CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        AuthTokens authTokens = refreshAccessTokenUseCase.execute(new RefreshAccessTokenCommand(refreshToken));

        ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());

        return ResponseEntity.ok().build();
    }

    @PreAuthorize("hasRole('UNVERIFIED')")
    @SecurityRequirement(name = "accessToken")
    @PostMapping("/email-verifications/confirm")
    public ResponseEntity<AfterLoginResponse> verifyEmail(@RequestBody @Valid EmailVerificationCodeRequest emailVerificationCodeRequest,
                                                          HttpServletResponse response) {
        CurrentUserDto user = getAuthenticatedUserDataUseCase.execute(new GetAuthenticatedUserCommand());

        LoginUserResult result = verifyEmailUseCase.execute(new VerifyEmailCommand(
                user.userId(), emailVerificationCodeRequest.code()
        ));

        setTokenCookies(result.authTokens(), response);

        return ResponseEntity.ok(AfterLoginResponse.from(result));
    }

    @PreAuthorize("hasRole('UNVERIFIED')")
    @SecurityRequirement(name = "accessToken")
    @PostMapping("/email-verifications/resend")
    public ResponseEntity<Void> resendVerificationCode() {
        CurrentUserDto user = getAuthenticatedUserDataUseCase.execute(new GetAuthenticatedUserCommand());

        resendEmailVerificationCodeUseCase.execute(new ResendEmailVerificationCodeCommand(user.userId()));

        return ResponseEntity.accepted().build();
    }

    @PostMapping("/forgot-password")
    public ResponseEntity<Void> forgotPassword(@Valid @RequestBody ForgotPasswordRequest request) {
        sendForgotPasswordTokenUseCase.execute(new SendForgotPasswordCodeCommand(request.email()));

        return ResponseEntity.noContent().build();
    }

    @PostMapping("/reset-password")
    public ResponseEntity<Void> resetPassword(@Valid @RequestBody ResetPasswordRequest request,
                                              HttpServletResponse response) {
        resetPasswordUseCase.execute(new ResetPasswordCommand(
                request.code(), request.newPassword()
        ));

        invalidateTokenCookies(response);

        return ResponseEntity.noContent().build();
    }

    private void invalidateTokenCookies(HttpServletResponse response) {
        ResponseCookie accessTokenCookie = cookieUtil.invalidateAccessTokenCookie();
        ResponseCookie refreshTokenCookie = cookieUtil.invalidateRefreshTokenCookie();

        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());
    }

    private void setTokenCookies(AuthTokens authTokens, HttpServletResponse response) {
        ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
        ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());

        response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());
    }
}
