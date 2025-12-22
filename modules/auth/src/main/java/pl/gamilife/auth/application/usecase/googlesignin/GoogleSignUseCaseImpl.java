package pl.gamilife.auth.application.usecase.googlesignin;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.application.dto.GoogleSignInResult;
import pl.gamilife.auth.application.dto.GoogleUserDto;
import pl.gamilife.auth.application.service.OAuthService;
import pl.gamilife.auth.domain.model.UserOAuthProvider;
import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.UserProviderRepository;

import java.util.Map;
import java.util.Optional;

@Service
@Transactional
@AllArgsConstructor
public class GoogleSignUseCaseImpl implements GoogleSignInUseCase {

    private final OAuthService oAuthService;
    private final UserProviderRepository userProviderRepository;
    private final UserContext userContext;

    @Override
    public GoogleSignInResult execute(GoogleSignInCommand cmd) {
        Map<String, String> tokenResponse = oAuthService.exchangeCodeForTokens(cmd.code(), cmd.codeVerifier());
        GoogleUserDto googleUserDto = oAuthService.extractUserInfoFromIdToken(tokenResponse.get("id_token"));

        Optional<UserOAuthProvider> existingOAuthUser = userProviderRepository
                .findByProviderAndProviderId("google", googleUserDto.sub());

        if (existingOAuthUser.isPresent()) {
            return oAuthService.loginViaGoogle(existingOAuthUser.get().getUserId(), googleUserDto.email());
        }

        Optional<BasicUserDetails> user = userContext.getUserByEmail(googleUserDto.email());
        if (user.isPresent()) {
            // User exists, but not linked to Google
            BasicUserDetails existingUser = user.get();
            return new GoogleSignInResult(
                    GoogleSignInResult.LoginType.POSSIBLE_LINK,
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
