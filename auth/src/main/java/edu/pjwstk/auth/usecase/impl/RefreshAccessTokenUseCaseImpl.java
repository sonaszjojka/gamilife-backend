package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.api.user.exception.UserNotFoundException;
import edu.pjwstk.auth.exceptions.RefreshTokenExpiredException;
import edu.pjwstk.auth.exceptions.RefreshTokenRevokedException;
import edu.pjwstk.auth.exceptions.RefreshTokenUnknownException;
import edu.pjwstk.auth.models.RefreshToken;
import edu.pjwstk.auth.repository.JpaRefreshTokenRepository;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.auth.usecase.RefreshAccessTokenUseCase;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class RefreshAccessTokenUseCaseImpl implements RefreshAccessTokenUseCase {

    private final TokenService tokenService;
    private final JpaRefreshTokenRepository refreshTokenRepository;
    private final UserApi userApi;

    @Override
    @Transactional
    public AuthTokens execute(String refreshToken) {
        String hashedRefreshToken = tokenService.hashToken(refreshToken);
        RefreshToken existingRefreshToken = refreshTokenRepository
                .findByToken(hashedRefreshToken)
                .orElseThrow(() -> new RefreshTokenUnknownException("Refresh token not found"));

        if (existingRefreshToken.isRevoked()) {
            throw new RefreshTokenRevokedException("Refresh token has been revoked");
        }

        if (existingRefreshToken.getExpiresAt().isBefore(LocalDateTime.now())) {
            throw new RefreshTokenExpiredException("Refresh token has expired");
        }

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(existingRefreshToken.getUserId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new AuthTokens(
                tokenService.generateAccessToken(user.userId(), user.email()),
                refreshToken
        );
    }
}
