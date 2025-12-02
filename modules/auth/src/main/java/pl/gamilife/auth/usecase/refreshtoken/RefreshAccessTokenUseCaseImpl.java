package pl.gamilife.auth.usecase.refreshtoken;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.SecureUserInfoApiDto;
import pl.gamilife.auth.exception.domain.InvalidRefreshTokenException;
import pl.gamilife.auth.exception.domain.RefreshTokenExpiredException;
import pl.gamilife.auth.models.RefreshToken;
import pl.gamilife.auth.repository.JpaRefreshTokenRepository;
import pl.gamilife.auth.service.TokenService;
import pl.gamilife.infrastructure.core.exception.common.domain.UserNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;

@Service
@Transactional
@AllArgsConstructor
public class RefreshAccessTokenUseCaseImpl implements RefreshAccessTokenUseCase {

    private final TokenService tokenService;
    private final JpaRefreshTokenRepository refreshTokenRepository;
    private final UserApi userApi;

    @Override
    public AuthTokens execute(RefreshAccessTokenCommand cmd) {
        String hashedRefreshToken = tokenService.hashToken(cmd.refreshToken());
        RefreshToken existingRefreshToken = refreshTokenRepository
                .findByToken(hashedRefreshToken)
                .orElseThrow(() -> new InvalidRefreshTokenException("Refresh token not found"));

        if (existingRefreshToken.isRevoked() || existingRefreshToken.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new RefreshTokenExpiredException("Refresh token has expired");
        }

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(existingRefreshToken.getUserId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new AuthTokens(
                tokenService.generateAccessToken(user.userId(), user.email()),
                cmd.refreshToken()
        );
    }
}
