package edu.pjwstk.auth.usecase.refreshtoken;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.auth.exception.domain.InvalidRefreshTokenException;
import edu.pjwstk.auth.exception.domain.RefreshTokenExpiredException;
import edu.pjwstk.auth.models.RefreshToken;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
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
