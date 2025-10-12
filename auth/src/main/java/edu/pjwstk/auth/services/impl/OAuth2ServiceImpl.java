package edu.pjwstk.auth.services.impl;

import edu.pjwstk.auth.dto.service.*;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.exceptions.LinkedUserNotFoundException;
import edu.pjwstk.auth.exceptions.UserAlreadyLinkedToProviderException;
import edu.pjwstk.auth.persistence.repository.UserProviderRepository;
import edu.pjwstk.auth.services.AuthService;
import edu.pjwstk.auth.util.OAuth2CodeUtil;
import edu.pjwstk.auth.services.OAuth2Service;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

@Service
public class OAuth2ServiceImpl implements OAuth2Service {

    private final UserApi userApi;
    private final UserProviderRepository userProviderRepository;
    private final OAuth2CodeUtil oAuth2CodeUtil;
    private final AuthService authService;

    public OAuth2ServiceImpl(
            UserApi userApi,
            UserProviderRepository userProviderRepository,
            OAuth2CodeUtil oAuth2CodeUtil,
            AuthService authService
    ) {
        this.userApi = userApi;
        this.userProviderRepository = userProviderRepository;
        this.oAuth2CodeUtil = oAuth2CodeUtil;
        this.authService = authService;
    }

    @Override
    @Transactional
    public Optional<AuthTokens> linkNewOAuthAccount(LinkOAuthAccountDto linkOAuthAccountDto) {
        if (linkOAuthAccountDto.shouldLink()) {
            BasicUserInfoApiDto user = userApi.getUserById(linkOAuthAccountDto.userId())
                    .orElseThrow(() -> new LinkedUserNotFoundException("Local user to link to not found"));

            if (!authService.checkPassword(linkOAuthAccountDto.password(), user.password())) {
                throw new InvalidCredentialsException("Invalid password");
            }

            if (userProviderRepository.checkIfUserHasProvider(user.userId(), linkOAuthAccountDto.provider())) {
                throw new UserAlreadyLinkedToProviderException("User is already linked to this provider");
            }

            userProviderRepository.save(new UserOAuthProvider(
                    UUID.randomUUID(),
                    user.userId(),
                    linkOAuthAccountDto.provider(),
                    linkOAuthAccountDto.providerId()

            ));

            return Optional.of(authService.issueJwtTokens(user));
        }

        return Optional.empty();
    }

    @Override
    @Transactional
    public GoogleLoginDTO handleGoogleLogin(OAuthCodeDto oAuthCodeDto) {
        Map<String, String> tokenResponse = oAuth2CodeUtil.exchangeCodeForTokens(oAuthCodeDto.code(), oAuthCodeDto.codeVerifier());
        GoogleUserDto googleUserDto = oAuth2CodeUtil.extractUserInfoFromIdToken(tokenResponse.get("id_token"));

        Optional<UserOAuthProvider> existingOAuthUser = userProviderRepository
                .getOAuthUserByProviderAndProviderId("google", googleUserDto.sub());

        if (existingOAuthUser.isPresent()) {
            // User already exists with this Google provider ID
            BasicUserInfoApiDto user = userApi.getUserById(existingOAuthUser.get().userId()).get();

            // Update email only if the user has no local account set up and the email is different
            if (user.password() == null && !user.email().equals(googleUserDto.email())) {
                userApi.updateUserEmail(user.userId(), googleUserDto.email());
            }

            AuthTokens authTokens = authService.issueJwtTokens(user);
            return new GoogleLoginDTO(
                    GoogleLoginDTO.LoginType.EXISTING_USER,
                    authTokens.accessToken(),
                    authTokens.refreshToken()
            );
        }

        Optional<BasicUserInfoApiDto> user = userApi.getUserByEmail(googleUserDto.email());

        if (user.isPresent()) {
            // User exists, but not linked to Google
            BasicUserInfoApiDto existingUser = user.get();
            return new GoogleLoginDTO(
                    GoogleLoginDTO.LoginType.POSSIBLE_LINK,
                    "google",
                    googleUserDto.sub(),
                    existingUser.userId()
            );
        } else {
            // User does not exist, create a new OAuth user
            return registerNewOAuth2User(googleUserDto);
        }
    }

    private GoogleLoginDTO registerNewOAuth2User(GoogleUserDto googleUserDto) {
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
                false // Default to private profile
        );
        BasicUserInfoApiDto createdGoogleUser = userApi.registerNewUser(newGoogleUser);

        userProviderRepository.save(new UserOAuthProvider(
                UUID.randomUUID(),
                createdGoogleUser.userId(),
                "google",
                googleUserDto.sub()
        ));

        AuthTokens authTokens = authService.issueJwtTokens(createdGoogleUser);

        return new GoogleLoginDTO(
                GoogleLoginDTO.LoginType.NEW_USER,
                authTokens.accessToken(),
                authTokens.refreshToken()
        );
    }
}
