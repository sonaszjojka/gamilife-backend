package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.exceptions.RefreshTokenNotProvidedException;
import edu.pjwstk.auth.exceptions.RefreshTokenUnknownException;
import edu.pjwstk.auth.models.RefreshTokenEntity;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.usecase.LogoutUserUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class LogoutUserUseCaseImpl implements LogoutUserUseCase {

    private final TokenProvider tokenProvider;
    private final JpaRefreshTokenRepository refreshTokenRepository;

    @Override
    public void execute(String refreshToken) {
        if (refreshToken.isBlank()) {
            throw new RefreshTokenNotProvidedException("Refresh token is blank value");
        }
        String hashedToken = tokenProvider.hashToken(refreshToken);

        RefreshTokenEntity refreshTokenFromDb = refreshTokenRepository
                .findByToken(hashedToken)
                .orElseThrow(() -> new RefreshTokenUnknownException("Refresh token not found"));

        if (!refreshTokenFromDb.getExpiresAt().isBefore(LocalDateTime.now()) && !refreshTokenFromDb.isRevoked()) {
            refreshTokenRepository.updateRevokedById(refreshTokenFromDb.getId(), true);
        }
    }
}
