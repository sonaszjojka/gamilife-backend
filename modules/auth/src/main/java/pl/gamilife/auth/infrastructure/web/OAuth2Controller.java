package pl.gamilife.auth.infrastructure.web;

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
import pl.gamilife.auth.application.AuthTokens;
import pl.gamilife.auth.application.common.LoginUserResult;
import pl.gamilife.auth.application.googlelinkaccount.LinkGoogleAccountCommand;
import pl.gamilife.auth.application.googlelinkaccount.LinkGoogleAccountUseCase;
import pl.gamilife.auth.application.googlesignin.GoogleSignInCommand;
import pl.gamilife.auth.application.googlesignin.GoogleSignInResult;
import pl.gamilife.auth.application.googlesignin.GoogleSignInUseCase;
import pl.gamilife.auth.infrastructure.web.request.LinkOAuthAccountRequest;
import pl.gamilife.auth.infrastructure.web.request.OAuthCodeRequest;
import pl.gamilife.auth.infrastructure.web.response.AfterLoginResponse;
import pl.gamilife.auth.infrastructure.web.response.OAuth2LinkResponse;
import pl.gamilife.shared.web.util.CookieUtil;

import java.security.InvalidParameterException;
import java.util.Optional;

@SecurityRequirements
@AllArgsConstructor
@RestController
@RequestMapping("/api/v1/oauth2")
public class OAuth2Controller {

    private final CookieUtil cookieUtil;
    private final LinkGoogleAccountUseCase linkGoogleAccountUseCase;
    private final GoogleSignInUseCase googleSignInUseCase;

    @PostMapping("/link")
    public ResponseEntity<AfterLoginResponse> linkOAuthAccounts(@RequestBody @Valid LinkOAuthAccountRequest linkOAuthAccountRequest,
                                                                HttpServletResponse response) {
        if (!linkOAuthAccountRequest.validate()) {
            throw new InvalidParameterException("If shouldLink is true, provider, providerId, userId, and password must be provided.");
        }

        Optional<LoginUserResult> possibleResult = linkGoogleAccountUseCase.execute(new LinkGoogleAccountCommand(
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
        GoogleSignInResult googleSignInResult = googleSignInUseCase
                .execute(new GoogleSignInCommand(request.code(), request.codeVerifier()));

        return switch (googleSignInResult.getLoginType()) {
            case GoogleSignInResult.LoginType.NEW_USER, GoogleSignInResult.LoginType.EXISTING_USER -> {
                LoginUserResult result = googleSignInResult.getLoginUserResult();
                AuthTokens authTokens = result.authTokens();
                ResponseCookie accessTokenCookie = cookieUtil.createAccessTokenCookie(authTokens.accessToken());
                ResponseCookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(authTokens.refreshToken());

                response.addHeader(HttpHeaders.SET_COOKIE, accessTokenCookie.toString());
                response.addHeader(HttpHeaders.SET_COOKIE, refreshTokenCookie.toString());

                yield ResponseEntity.ok(AfterLoginResponse.from(result));
            }
            case GoogleSignInResult.LoginType.POSSIBLE_LINK -> ResponseEntity.status(HttpStatus.CONFLICT)
                    .body(new OAuth2LinkResponse(
                            googleSignInResult.getProviderName(),
                            googleSignInResult.getProviderId(),
                            googleSignInResult.getUserId()
                    ));
        };
    }
}
