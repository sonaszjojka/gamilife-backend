package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.models.RefreshTokenEntity;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.usecase.GenerateAuthTokenPairUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.common.authApi.dto.AuthTokens;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GenerateAuthTokenPairUseCaseImpl implements GenerateAuthTokenPairUseCase {

    private final TokenProvider tokenProvider;
    private final JpaRefreshTokenRepository refreshTokenRepository;

    @Override
    @Transactional
    public AuthTokens execute(UUID userId, String email, boolean isEmailVerified) {
        AuthTokens authTokens = tokenProvider.generateTokenPair(userId, email, isEmailVerified);

        // Save hashed refresh token to database
        refreshTokenRepository.save(new RefreshTokenEntity(
                UUID.randomUUID(),
                userId,
                tokenProvider.hashToken(authTokens.refreshToken()),
                LocalDateTime.now(),
                LocalDateTime.now().plusSeconds(tokenProvider.getRefreshTokenExpirationTime()),
                false
        ));

        return authTokens;
    }
}
