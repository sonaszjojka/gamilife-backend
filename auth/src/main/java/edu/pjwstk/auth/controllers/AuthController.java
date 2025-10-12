package edu.pjwstk.auth.controllers;

import edu.pjwstk.auth.dto.request.LoginUserRequest;
import edu.pjwstk.auth.dto.request.RegisterUserRequest;
import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.auth.usecase.LoginUserUseCase;
import edu.pjwstk.auth.usecase.LogoutUserUseCase;
import edu.pjwstk.auth.usecase.RefreshAccessTokenUseCase;
import edu.pjwstk.auth.usecase.RegisterUserUseCase;
import edu.pjwstk.auth.util.CookieUtil;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.security.SecurityRequirements;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
    public ResponseEntity<Void> loginUser(@RequestBody @Valid LoginUserRequest request,
                                          HttpServletResponse response) {
        AuthTokens authTokens = loginUserUseCase.execute(
                new LoginUserDto(
                        request.email(),
                        request.password()
                )
        );

        Cookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());
        Cookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());

        response.addCookie(refreshTokenCookie);
        response.addCookie(accessTokenCookie);

        return ResponseEntity.ok().build();
    }

    @PostMapping("/logout")
    public ResponseEntity<Void> logout(@CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        logoutUserUseCase.execute(refreshToken);

        response.addCookie(cookieUtil.invalidateAccessTokenCookie());
        response.addCookie(cookieUtil.invalidateRefreshTokenCookie());

        return ResponseEntity.ok().build();
    }

    @SecurityRequirement(name = "refreshToken")
    @PostMapping("/refresh")
    public ResponseEntity<Void> refreshTokens(
            @CookieValue(name = "REFRESH-TOKEN") String refreshToken, HttpServletResponse response) {
        AuthTokens authTokens = refreshAccessTokenUseCase.execute(refreshToken);

        Cookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
        response.addCookie(accessTokenCookie);

        return ResponseEntity.ok().build();
    }
}
