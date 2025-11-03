package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;
import edu.pjwstk.auth.dto.service.LoginUserResult;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.exceptions.LinkedUserNotFoundException;
import edu.pjwstk.auth.exceptions.UserAlreadyLinkedToProviderException;
import edu.pjwstk.auth.models.UserOAuthProviderEntity;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.usecase.LinkNewOAuthAccountUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.SecureUserInfoApiDto;
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
    private final TokenProvider tokenProvider;

    @Override
    @Transactional
    public Optional<LoginUserResult> execute(LinkOAuthAccountDto linkOAuthAccountDto) {
        if (!linkOAuthAccountDto.shouldLink()) {
            return Optional.empty();
        }

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(linkOAuthAccountDto.userId())
                .orElseThrow(() -> new LinkedUserNotFoundException("Local user to link to not found"));

        if (!passwordEncoder.matches(linkOAuthAccountDto.password(), user.password())) {
            throw new InvalidCredentialsException("Invalid password");
        }

        if (userProviderRepository.existsByUserIdAndProvider(user.userId(), linkOAuthAccountDto.provider())) {
            throw new UserAlreadyLinkedToProviderException("User is already linked to this provider");
        }

        userProviderRepository.save(new UserOAuthProviderEntity(
                UUID.randomUUID(),
                user.userId(),
                linkOAuthAccountDto.provider(),
                linkOAuthAccountDto.providerId()

        ));

        // Verify use email address if not verified
        if (!user.isEmailVerified()) {
            userApi.confirmUserEmailVerification(user.userId());
        }

        AuthTokens tokens = tokenProvider.generateTokenPair(user.userId(), user.email(), true);

        return Optional.of(new LoginUserResult(
                user.userId(),
                user.email(),
                user.username(),
                true,
                tokens
        ));
    }
}
