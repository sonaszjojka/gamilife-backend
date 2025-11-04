package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.auth.usecase.command.LinkNewOAuthAccountCommand;
import edu.pjwstk.auth.usecase.result.LoginUserResult;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.exceptions.LinkedUserNotFoundException;
import edu.pjwstk.auth.exceptions.UserAlreadyLinkedToProviderException;
import edu.pjwstk.auth.models.UserOAuthProvider;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.usecase.LinkNewOAuthAccountUseCase;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class LinkNewOAuthAccountUseCaseImpl implements LinkNewOAuthAccountUseCase {

    private final UserApi userApi;
    private final PasswordEncoder passwordEncoder;
    private final JpaUserProviderRepository userProviderRepository;
    private final TokenService tokenService;

    @Override
    @Transactional
    public Optional<LoginUserResult> execute(LinkNewOAuthAccountCommand linkNewOAuthAccountCommand) {
        if (!linkNewOAuthAccountCommand.shouldLink()) {
            return Optional.empty();
        }

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(linkNewOAuthAccountCommand.userId())
                .orElseThrow(() -> new LinkedUserNotFoundException("Local user to link to not found"));

        if (!passwordEncoder.matches(linkNewOAuthAccountCommand.password(), user.password())) {
            throw new InvalidCredentialsException("Invalid password");
        }

        if (userProviderRepository.existsByUserIdAndProvider(user.userId(), linkNewOAuthAccountCommand.provider())) {
            throw new UserAlreadyLinkedToProviderException("User is already linked to this provider");
        }

        userProviderRepository.save(new UserOAuthProvider(
                UUID.randomUUID(),
                user.userId(),
                linkNewOAuthAccountCommand.provider(),
                linkNewOAuthAccountCommand.providerId()

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
                tokens
        ));
    }
}
