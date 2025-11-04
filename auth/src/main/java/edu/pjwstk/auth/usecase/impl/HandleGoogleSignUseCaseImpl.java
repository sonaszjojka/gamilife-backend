package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.auth.dto.service.GoogleUserDto;
import edu.pjwstk.auth.models.UserOAuthProvider;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.service.OAuthService;
import edu.pjwstk.auth.usecase.HandleGoogleSignInUseCase;
import edu.pjwstk.auth.usecase.command.HandleGoogleSignInCommand;
import edu.pjwstk.auth.usecase.result.GoogleLoginResult;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Map;
import java.util.Optional;

@Service
@AllArgsConstructor
public class HandleGoogleSignUseCaseImpl implements HandleGoogleSignInUseCase {

    private final OAuthService oAuthService;
    private final JpaUserProviderRepository userProviderRepository;
    private final UserApi userApi;

    @Override
    @Transactional
    public GoogleLoginResult execute(HandleGoogleSignInCommand handleGoogleSignInCommand) {
        Map<String, String> tokenResponse = oAuthService.exchangeCodeForTokens(handleGoogleSignInCommand.code(), handleGoogleSignInCommand.codeVerifier());
        GoogleUserDto googleUserDto = oAuthService.extractUserInfoFromIdToken(tokenResponse.get("id_token"));

        Optional<UserOAuthProvider> existingOAuthUser = userProviderRepository
                .findByProviderAndProviderId("google", googleUserDto.sub());

        if (existingOAuthUser.isPresent()) {
            return oAuthService.loginViaGoogle(existingOAuthUser.get().getUserId(), googleUserDto.email());
        }

        Optional<BasicUserInfoApiDto> user = userApi.getUserByEmail(googleUserDto.email());
        if (user.isPresent()) {
            // User exists, but not linked to Google
            BasicUserInfoApiDto existingUser = user.get();
            return new GoogleLoginResult(
                    GoogleLoginResult.LoginType.POSSIBLE_LINK,
                    "google",
                    googleUserDto.sub(),
                    existingUser.userId()
            );
        } else {
            // User does not exist, create a new OAuth user
            return oAuthService.registerViaGoogle(googleUserDto);
        }
    }
}
