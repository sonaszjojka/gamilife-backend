package pl.gamilife.auth.application.usecase.googlelinkaccount;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.exception.domain.InvalidCredentialsException;
import pl.gamilife.auth.domain.exception.domain.LinkedUserNotFoundException;
import pl.gamilife.auth.domain.exception.domain.UserAlreadyLinkedToProviderException;
import pl.gamilife.auth.domain.model.UserOAuthProvider;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.UserProviderRepository;

import java.util.Optional;

@Service
@Transactional
@AllArgsConstructor
public class LinkGoogleAccountUseCaseImpl implements LinkGoogleAccountUseCase {

    private final UserContext userContext;
    private final PasswordEncoder passwordEncoder;
    private final UserProviderRepository userProviderRepository;
    private final TokenService tokenService;

    @Override
    public Optional<LoginUserResult> execute(LinkGoogleAccountCommand cmd) {
        if (!cmd.shouldLink()) {
            return Optional.empty();
        }

        SecureUserDetails user = userContext.getSecureUserDataById(cmd.userId())
                .orElseThrow(() -> new LinkedUserNotFoundException("Local user to link to not found"));

        if (!passwordEncoder.matches(cmd.password(), user.password())) {
            throw new InvalidCredentialsException("Invalid password");
        }

        if (userProviderRepository.existsByUserIdAndProvider(user.userId(), cmd.provider())) {
            throw new UserAlreadyLinkedToProviderException("User is already linked to this provider");
        }

        userProviderRepository.save(UserOAuthProvider.create(
                user.userId(),
                cmd.provider(),
                cmd.providerId()
        ));

        // Verify use email address if not verified
        if (!user.isEmailVerified()) {
            userContext.confirmUserEmailVerification(user.userId());
        }

        AuthTokens tokens = tokenService.generateTokenPair(user.userId(), user.email(), true);

        return Optional.of(new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                true,
                user.isTutorialCompleted(),
                user.money(),
                tokens
        ));
    }
}
