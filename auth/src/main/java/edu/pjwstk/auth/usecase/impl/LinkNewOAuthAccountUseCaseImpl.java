package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;
import edu.pjwstk.auth.dto.service.UserOAuthProvider;
import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.exceptions.LinkedUserNotFoundException;
import edu.pjwstk.auth.exceptions.UserAlreadyLinkedToProviderException;
import edu.pjwstk.auth.persistence.repository.UserProviderRepository;
import edu.pjwstk.auth.usecase.LinkNewOAuthAccountUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
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
    private final UserProviderRepository userProviderRepository;
    private final TokenProvider tokenProvider;

    @Override
    @Transactional
    public Optional<AuthTokens> execute(LinkOAuthAccountDto linkOAuthAccountDto) {
        if (!linkOAuthAccountDto.shouldLink()) {
            return Optional.empty();
        }

        BasicUserInfoApiDto user = userApi.getUserById(linkOAuthAccountDto.userId())
                .orElseThrow(() -> new LinkedUserNotFoundException("Local user to link to not found"));

        if (!passwordEncoder.matches(linkOAuthAccountDto.password(), user.password())) {
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

        return Optional.of(tokenProvider.generateTokenPair(user.userId(), user.email()));
    }
}
