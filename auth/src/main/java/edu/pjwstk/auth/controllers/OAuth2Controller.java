package edu.pjwstk.auth.controllers;

import edu.pjwstk.auth.dto.request.LinkOAuthAccountRequest;
import edu.pjwstk.auth.dto.request.OAuthCodeRequest;
import edu.pjwstk.auth.dto.response.AfterLoginResponse;
import edu.pjwstk.auth.dto.response.OAuth2LinkResponse;
import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;
import edu.pjwstk.auth.dto.service.OAuthCodeDto;
import edu.pjwstk.auth.usecase.HandleGoogleSignInUseCase;
import edu.pjwstk.auth.usecase.LinkNewOAuthAccountUseCase;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.commonweb.CookieUtil;
import io.swagger.v3.oas.annotations.security.SecurityRequirements;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.InvalidParameterException;
import java.util.Optional;

@SecurityRequirements
@AllArgsConstructor
@RestController
@RequestMapping("/api/v1/oauth2")
public class OAuth2Controller {

    private final CookieUtil cookieUtil;
    private final LinkNewOAuthAccountUseCase linkNewOAuthAccountUseCase;
    private final HandleGoogleSignInUseCase handleGoogleSignInUseCase;

    @PostMapping("/link")
    public ResponseEntity<AfterLoginResponse> linkOAuthAccounts(@RequestBody @Valid LinkOAuthAccountRequest linkOAuthAccountRequest,
                                                                HttpServletResponse response) {
        if (!linkOAuthAccountRequest.validate()) {
            throw new InvalidParameterException("If shouldLink is true, provider, providerId, userId, and password must be provided.");
        }

        Optional<AuthTokens> authTokens = linkNewOAuthAccountUseCase.execute(new LinkOAuthAccountDto(
                linkOAuthAccountRequest.shouldLink(),
                linkOAuthAccountRequest.provider(),
                linkOAuthAccountRequest.providerId(),
                linkOAuthAccountRequest.userId(),
                linkOAuthAccountRequest.password()
        ));

        if (authTokens.isPresent()) {
            AuthTokens createdTokens = authTokens.get();

            ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(createdTokens.accessToken());
            ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(createdTokens.refreshToken());

            response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
            response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

            return ResponseEntity.ok(new AfterLoginResponse(authTokens.get().isEmailVerified()));
        }

        return ResponseEntity.noContent().build();
    }

    @PostMapping("/code/google")
    public ResponseEntity<?> handleGoogleCode(@RequestBody OAuthCodeRequest request,
                                              HttpServletResponse response) {
        GoogleLoginDTO googleLoginDTO = handleGoogleSignInUseCase
                .execute(new OAuthCodeDto(request.code(), request.codeVerifier()));

        return switch (googleLoginDTO.getLoginType()) {
            case GoogleLoginDTO.LoginType.NEW_USER, GoogleLoginDTO.LoginType.EXISTING_USER -> {
                AuthTokens authTokens = googleLoginDTO.getAuthTokens();
                ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
                ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());

                response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
                response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

                yield ResponseEntity.ok(new AfterLoginResponse(authTokens.isEmailVerified()));
            }
            case GoogleLoginDTO.LoginType.POSSIBLE_LINK -> ResponseEntity.status(HttpStatus.CONFLICT)
                    .body(new OAuth2LinkResponse(
                            googleLoginDTO.getProviderName(),
                            googleLoginDTO.getProviderId(),
                            googleLoginDTO.getUserId()
                    ));
        };
    }
}
