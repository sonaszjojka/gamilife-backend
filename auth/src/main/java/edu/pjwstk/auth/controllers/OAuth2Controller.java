package edu.pjwstk.auth.controllers;

import edu.pjwstk.auth.dto.request.LinkOAuthAccountRequest;
import edu.pjwstk.auth.dto.request.OAuthCodeRequest;
import edu.pjwstk.auth.dto.response.OAuth2LinkResponse;
import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;
import edu.pjwstk.auth.dto.service.OAuthCodeDto;
import edu.pjwstk.auth.services.OAuth2Service;
import edu.pjwstk.auth.util.CookieUtil;
import io.swagger.v3.oas.annotations.security.SecurityRequirements;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.security.InvalidParameterException;
import java.util.Optional;

@SecurityRequirements
@RestController
@RequestMapping("/api/v1/oauth2")
public class OAuth2Controller {

    private final OAuth2Service oAuth2Service;
    private final CookieUtil cookieUtil;

    public OAuth2Controller(OAuth2Service oAuth2Service, CookieUtil cookieUtil) {
        this.oAuth2Service = oAuth2Service;
        this.cookieUtil = cookieUtil;
    }

    @PostMapping("/link")
    public ResponseEntity<Void> linkOAuthAccounts(@RequestBody @Valid LinkOAuthAccountRequest linkOAuthAccountRequest,
                                                  HttpServletResponse response) {
        if (!linkOAuthAccountRequest.validate()) {
            throw new InvalidParameterException("If shouldLink is true, provider, providerId, userId, and password must be provided.");
        }

        Optional<AuthTokens> authTokens = oAuth2Service.linkNewOAuthAccount(new LinkOAuthAccountDto(
                linkOAuthAccountRequest.shouldLink(),
                linkOAuthAccountRequest.provider(),
                linkOAuthAccountRequest.providerId(),
                linkOAuthAccountRequest.userId(),
                linkOAuthAccountRequest.password()
        ));

        if (authTokens.isPresent()) {
            AuthTokens createdTokens = authTokens.get();

            Cookie refreshTokenCookie = cookieUtil.createRefreshTokenCookie(createdTokens.refreshToken());
            Cookie accessTokenCookie = cookieUtil.createAccessTokenCookie(createdTokens.accessToken());

            response.addCookie(refreshTokenCookie);
            response.addCookie(accessTokenCookie);

            return ResponseEntity.ok().build();
        }

        return ResponseEntity.noContent().build();
    }

    @PostMapping("/code/google")
    public ResponseEntity<?> handleGoogleCode(@RequestBody OAuthCodeRequest request,
                                              HttpServletResponse response) {
        GoogleLoginDTO googleLoginDTO = oAuth2Service
                .handleGoogleLogin(new OAuthCodeDto(request.code(), request.codeVerifier()));

        return switch (googleLoginDTO.getLoginType()) {
            case GoogleLoginDTO.LoginType.NEW_USER, GoogleLoginDTO.LoginType.EXISTING_USER -> {
                Cookie refreshTokenCookie = cookieUtil
                        .createRefreshTokenCookie(googleLoginDTO.getRefreshToken());

                Cookie accessTokenCookie = cookieUtil
                        .createAccessTokenCookie(googleLoginDTO.getAccessToken());

                response.addCookie(refreshTokenCookie);
                response.addCookie(accessTokenCookie);

                yield ResponseEntity.ok().build();
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
