package edu.pjwstk.auth.usecase.googlelinkaccount;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.InvalidCredentialsException;
import edu.pjwstk.auth.exception.domain.LinkedUserNotFoundException;
import edu.pjwstk.auth.exception.domain.UserAlreadyLinkedToProviderException;
import edu.pjwstk.auth.models.UserOAuthProvider;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.common.LoginUserResult;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class LinkGoogleAccountUseCaseImpl implements LinkGoogleAccountUseCase {

    private final UserApi userApi;
    private final PasswordEncoder passwordEncoder;
    private final JpaUserProviderRepository userProviderRepository;
    private final TokenService tokenService;

    @Override
    @Transactional
    public Optional<LoginUserResult> executeInternal(LinkGoogleAccountCommand cmd) {
        if (!cmd.shouldLink()) {
            return Optional.empty();
        }

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(cmd.userId())
                .orElseThrow(() -> new LinkedUserNotFoundException("Local user to link to not found"));

        if (!passwordEncoder.matches(cmd.password(), user.password())) {
            throw new InvalidCredentialsException("Invalid password");
        }

        if (userProviderRepository.existsByUserIdAndProvider(user.userId(), cmd.provider())) {
            throw new UserAlreadyLinkedToProviderException("User is already linked to this provider");
        }

        userProviderRepository.save(new UserOAuthProvider(
                UUID.randomUUID(),
                user.userId(),
                cmd.provider(),
                cmd.providerId()

        ));

        // Verify use email address if not verified
        if (!user.isEmailVerified()) {
            userApi.confirmUserEmailVerification(user.userId());
        }

        AuthTokens tokens = tokenService.generateTokenPair(user.userId(), user.email(), true);

        return Optional.of(new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                true,
                user.isTutorialCompleted(),
                tokens
        ));
    }
}
