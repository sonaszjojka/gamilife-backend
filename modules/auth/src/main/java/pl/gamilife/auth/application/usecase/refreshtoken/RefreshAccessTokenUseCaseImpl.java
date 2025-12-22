package pl.gamilife.auth.application.usecase.refreshtoken;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.exception.domain.InvalidRefreshTokenException;
import pl.gamilife.auth.domain.exception.domain.RefreshTokenExpiredException;
import pl.gamilife.auth.domain.model.RefreshToken;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.port.repository.RefreshTokenRepository;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;

import java.time.LocalDateTime;

@Service
@Transactional
@AllArgsConstructor
public class RefreshAccessTokenUseCaseImpl implements RefreshAccessTokenUseCase {

    private final TokenService tokenService;
    private final RefreshTokenRepository refreshTokenRepository;
    private final UserContext userContext;

    @Override
    public AuthTokens execute(RefreshAccessTokenCommand cmd) {
        String hashedRefreshToken = tokenService.hashToken(cmd.refreshToken());
        RefreshToken existingRefreshToken = refreshTokenRepository
                .findByToken(hashedRefreshToken)
                .orElseThrow(() -> new InvalidRefreshTokenException("Refresh token not found"));

        if (existingRefreshToken.isRevoked() || existingRefreshToken.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new RefreshTokenExpiredException("Refresh token has expired");
        }

        SecureUserDetails user = userContext.getSecureUserDataById(existingRefreshToken.getUserId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new AuthTokens(
                tokenService.generateAccessToken(user.userId(), user.email(), user.isEmailVerified()),
                cmd.refreshToken()
        );
    }
}
