package pl.gamilife.auth.application.logout;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.domain.exception.domain.InvalidRefreshTokenException;
import pl.gamilife.auth.domain.model.RefreshToken;
import pl.gamilife.auth.domain.port.repository.RefreshTokenRepository;
import pl.gamilife.auth.service.TokenService;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class LogoutUserUseCaseImpl implements LogoutUserUseCase {

    private final TokenService tokenService;
    private final RefreshTokenRepository refreshTokenRepository;

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
