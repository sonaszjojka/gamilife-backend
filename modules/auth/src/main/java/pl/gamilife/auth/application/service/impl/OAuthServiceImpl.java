package pl.gamilife.auth.application.service.impl;

import com.auth0.jwt.JWT;
import com.auth0.jwt.interfaces.DecodedJWT;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.application.dto.GoogleSignInResult;
import pl.gamilife.auth.application.dto.GoogleUserDto;
import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.auth.application.port.GoogleAuthClient;
import pl.gamilife.auth.application.service.OAuthService;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.model.UserOAuthProvider;
import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.model.projection.RegisterUserDetails;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.UserProviderRepository;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

@Service
@AllArgsConstructor
public class OAuthServiceImpl implements OAuthService {

    private final TokenService tokenService;
    private final UserContext userContext;
    private final UserProviderRepository userProviderRepository;
    private final GoogleAuthClient googleAuthClient;

    @Override
    public GoogleUserDto exchangeCodeForTokens(String code, String codeVerifier) {
        Map<String, String> response = googleAuthClient.call(code, codeVerifier);
        String token = response.get("id_token");
        DecodedJWT jwt = JWT.decode(token);

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
        SecureUserDetails user = userContext.getSecureUserDataById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Update email only if the user has no local account set up and the email is different
        if (user.password() == null && !user.email().equals(googleEmail)) {
            userContext.updateUserEmail(user.userId(), googleEmail);
        }

        return new GoogleSignInResult(
                GoogleSignInResult.LoginType.EXISTING_USER,
                new LoginUserResult(
                        user.userId(),
                        user.email(),
                        user.username(),
                        true,
                        user.isTutorialCompleted(),
                        user.money(),
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

        RegisterUserDetails newGoogleUser = new RegisterUserDetails(
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
        BasicUserDetails createdGoogleUser = userContext.registerNewUser(newGoogleUser);

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
                        createdGoogleUser.money(),
                        tokenService.generateTokenPair(
                                createdGoogleUser.userId(),
                                createdGoogleUser.email(),
                                true
                        )
                )
        );
    }


}
