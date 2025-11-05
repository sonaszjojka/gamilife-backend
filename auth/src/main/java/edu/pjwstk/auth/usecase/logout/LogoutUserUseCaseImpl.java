package edu.pjwstk.auth.usecase.logout;

import edu.pjwstk.auth.exceptions.RefreshTokenUnknownException;
import edu.pjwstk.auth.models.RefreshToken;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.service.TokenService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class LogoutUserUseCaseImpl implements LogoutUserUseCase {

    private final TokenService tokenService;
    private final JpaRefreshTokenRepository refreshTokenRepository;

    @Override
    public Void executeInternal(LogoutUserCommand cmd) {
        String hashedToken = tokenService.hashToken(cmd.refreshToken());

        RefreshToken refreshTokenFromDb = refreshTokenRepository
                .findByToken(hashedToken)
                .orElseThrow(() -> new RefreshTokenUnknownException("Refresh token not found"));

        if (!refreshTokenFromDb.getExpiresAt().isBefore(LocalDateTime.now()) && !refreshTokenFromDb.isRevoked()) {
            refreshTokenRepository.updateRevokedById(refreshTokenFromDb.getId(), true);
        }

        return null;
    }
}
