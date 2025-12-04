package pl.gamilife.auth.usecase.logout;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.exception.domain.InvalidRefreshTokenException;
import pl.gamilife.auth.models.RefreshToken;
import pl.gamilife.auth.repository.JpaRefreshTokenRepository;
import pl.gamilife.auth.service.TokenService;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class LogoutUserUseCaseImpl implements LogoutUserUseCase {

    private final TokenService tokenService;
    private final JpaRefreshTokenRepository refreshTokenRepository;

    @Override
    public Void execute(LogoutUserCommand cmd) {
        String hashedToken = tokenService.hashToken(cmd.refreshToken());

        RefreshToken refreshTokenFromDb = refreshTokenRepository
                .findByToken(hashedToken)
                .orElseThrow(() -> new InvalidRefreshTokenException("Refresh token not found"));

        if (!refreshTokenFromDb.getExpiresAt().isBefore(LocalDateTime.now()) && !refreshTokenFromDb.isRevoked()) {
            refreshTokenRepository.updateRevokedById(refreshTokenFromDb.getId(), true);
        }

        return null;
    }
}
