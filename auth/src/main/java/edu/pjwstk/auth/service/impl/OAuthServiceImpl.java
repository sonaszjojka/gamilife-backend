package edu.pjwstk.auth.service.impl;

import com.auth0.jwt.JWT;
import com.auth0.jwt.interfaces.DecodedJWT;
import pl.gamification.api.user.UserApi;
import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import pl.gamification.api.user.dto.RegisterUserApiDto;
import pl.gamification.api.user.dto.SecureUserInfoApiDto;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.auth.dto.GoogleUserDto;
import edu.pjwstk.auth.models.UserOAuthProvider;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.service.OAuthService;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import edu.pjwstk.auth.usecase.googlesignin.GoogleSignInResult;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

@Service
public class OAuthServiceImpl implements OAuthService {

    private final WebClient webClient;
    private final TokenService tokenService;
    private final UserApi userApi;
    private final JpaUserProviderRepository userProviderRepository;

    @Value("${spring.security.oauth2.client.registration.google.client-id}")
    private String googleClientId;

    @Value("${spring.security.oauth2.client.registration.google.client-secret}")
    private String googleClientSecret;

    @Value("${spring.security.oauth2.client.registration.google.redirect-uri}")
    private String googleRedirectUri;

    public OAuthServiceImpl(WebClient webClient, TokenService tokenService, UserApi userApi, JpaUserProviderRepository userProviderRepository) {
        this.webClient = webClient;
        this.tokenService = tokenService;
        this.userApi = userApi;
        this.userProviderRepository = userProviderRepository;
    }

    @Override
    public Map<String, String> exchangeCodeForTokens(String code, String codeVerifier) {
        MultiValueMap<String, String> formData = new LinkedMultiValueMap<>();
        formData.add("grant_type", "authorization_code");
        formData.add("code", code);
        formData.add("client_id", googleClientId);
        formData.add("client_secret", googleClientSecret);
        formData.add("redirect_uri", googleRedirectUri);
        formData.add("code_verifier", codeVerifier);

        return webClient.post()
                .uri("https://oauth2.googleapis.com/token")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_FORM_URLENCODED_VALUE)
                .body(BodyInserters.fromFormData(formData))
                .retrieve()
                .bodyToMono(new ParameterizedTypeReference<Map<String, String>>() {
                })
                .block();
    }

    @Override
    public GoogleUserDto extractUserInfoFromIdToken(String idToken) {
        DecodedJWT jwt = JWT.decode(idToken);

        return new GoogleUserDto(
                jwt.getClaim("sub").asString(),
                jwt.getClaim("email").asString(),
                jwt.getClaim("given_name").asString(),
                jwt.getClaim("family_name").asString()
        );
    }

    @Override
    public GoogleSignInResult loginViaGoogle(UUID userId, String googleEmail) {
        // User already exists with this Google provider ID
        SecureUserInfoApiDto user = userApi.getSecureUserDataById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Update email only if the user has no local account set up and the email is different
        if (user.password() == null && !user.email().equals(googleEmail)) {
            userApi.updateUserEmail(user.userId(), googleEmail);
        }

        return new GoogleSignInResult(
                GoogleSignInResult.LoginType.EXISTING_USER,
                new LoginUserResult(
                        user.userId(),
                        user.email(),
                        user.username(),
                        true,
                        user.isTutorialCompleted(),
                        tokenService.generateTokenPair(user.userId(), user.email(), true)
                )
        );
    }

    @Override
    @Transactional
    public GoogleSignInResult registerViaGoogle(GoogleUserDto googleUserDto) {
        String username = googleUserDto.firstName().substring(0, 3).toLowerCase() + "_" +
                googleUserDto.lastName().substring(0, 3).toLowerCase() + "_" +
                ThreadLocalRandom.current().nextInt(1000, 9999);

        RegisterUserApiDto newGoogleUser = new RegisterUserApiDto(
                googleUserDto.firstName(),
                googleUserDto.lastName(),
                googleUserDto.email(),
                null,
                username,
                null, // No birthdate provided
                false, // Default to not sending budget reports
                false, // Default to private profile
                true,
                false // Default to no onboarding completed
        );
        BasicUserInfoApiDto createdGoogleUser = userApi.registerNewUser(newGoogleUser);

        userProviderRepository.save(new UserOAuthProvider(
                UUID.randomUUID(),
                createdGoogleUser.userId(),
                "google",
                googleUserDto.sub()
        ));


        return new GoogleSignInResult(
                GoogleSignInResult.LoginType.NEW_USER,
                new LoginUserResult(
                        createdGoogleUser.userId(),
                        createdGoogleUser.email(),
                        createdGoogleUser.username(),
                        true,
                        false,
                        tokenService.generateTokenPair(
                                createdGoogleUser.userId(),
                                createdGoogleUser.email(),
                                true
                        )
                )
        );
    }


}
