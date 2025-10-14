package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.RefreshToken;
import edu.pjwstk.auth.exceptions.RefreshTokenNotProvidedException;
import edu.pjwstk.auth.exceptions.RefreshTokenUnknownException;
import edu.pjwstk.auth.persistence.repository.RefreshTokenRepository;
import edu.pjwstk.auth.usecase.LogoutUserUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class LogoutUserUseCaseImpl implements LogoutUserUseCase {

    private final TokenProvider tokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;

    @Override
    public void execute(String refreshToken) {
        if (refreshToken.isBlank()) {
            throw new RefreshTokenNotProvidedException("Refresh token is blank value");
        }
        String hashedToken = tokenProvider.hashToken(refreshToken);

        RefreshToken refreshTokenFromDb = refreshTokenRepository
                .getRefreshTokenByHashedToken(hashedToken)
                .orElseThrow(() -> new RefreshTokenUnknownException("Refresh token not found"));

        if (!refreshTokenFromDb.expiresAt().isBefore(LocalDateTime.now()) && !refreshTokenFromDb.revoked()) {
            refreshTokenRepository.updateRevokedStatus(refreshTokenFromDb.id(), true);
        }
    }
}
