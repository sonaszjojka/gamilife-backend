package edu.pjwstk.auth.controller;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.controller.request.LinkOAuthAccountRequest;
import edu.pjwstk.auth.controller.request.OAuthCodeRequest;
import edu.pjwstk.auth.controller.response.AfterLoginResponse;
import edu.pjwstk.auth.controller.response.OAuth2LinkResponse;
import edu.pjwstk.auth.usecase.HandleGoogleSignInUseCase;
import edu.pjwstk.auth.usecase.LinkNewOAuthAccountUseCase;
import edu.pjwstk.auth.usecase.command.HandleGoogleSignInCommand;
import edu.pjwstk.auth.usecase.command.LinkNewOAuthAccountCommand;
import edu.pjwstk.auth.usecase.result.GoogleLoginResult;
import edu.pjwstk.auth.usecase.result.LoginUserResult;
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

        Optional<LoginUserResult> possibleResult = linkNewOAuthAccountUseCase.execute(new LinkNewOAuthAccountCommand(
                linkOAuthAccountRequest.shouldLink(),
                linkOAuthAccountRequest.provider(),
                linkOAuthAccountRequest.providerId(),
                linkOAuthAccountRequest.userId(),
                linkOAuthAccountRequest.password()
        ));

        if (possibleResult.isPresent()) {
            LoginUserResult result = possibleResult.get();
            AuthTokens authTokens = result.authTokens();

            ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
            ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());

            response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
            response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

            return ResponseEntity.ok(AfterLoginResponse.from(result));
        }

        return ResponseEntity.noContent().build();
    }

    @PostMapping("/code/google")
    public ResponseEntity<?> handleGoogleCode(@RequestBody OAuthCodeRequest request,
                                              HttpServletResponse response) {
        GoogleLoginResult googleLoginResult = handleGoogleSignInUseCase
                .execute(new HandleGoogleSignInCommand(request.code(), request.codeVerifier()));

        return switch (googleLoginResult.getLoginType()) {
            case GoogleLoginResult.LoginType.NEW_USER, GoogleLoginResult.LoginType.EXISTING_USER -> {
                LoginUserResult result = googleLoginResult.getLoginUserResult();
                AuthTokens authTokens = result.authTokens();
                ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
                ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());

                response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
                response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

                yield ResponseEntity.ok(AfterLoginResponse.from(result));
            }
            case GoogleLoginResult.LoginType.POSSIBLE_LINK -> ResponseEntity.status(HttpStatus.CONFLICT)
                    .body(new OAuth2LinkResponse(
                            googleLoginResult.getProviderName(),
                            googleLoginResult.getProviderId(),
                            googleLoginResult.getUserId()
                    ));
        };
    }
}
