package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.RefreshToken;
import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.exceptions.RefreshTokenExpiredException;
import edu.pjwstk.auth.exceptions.RefreshTokenRevokedException;
import edu.pjwstk.auth.exceptions.RefreshTokenUnknownException;
import edu.pjwstk.auth.persistence.repository.RefreshTokenRepository;
import edu.pjwstk.auth.usecase.RefreshAccessTokenUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.SecureUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Service
@AllArgsConstructor
public class RefreshAccessTokenUseCaseImpl implements RefreshAccessTokenUseCase {

    private final TokenProvider tokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    private final UserApi userApi;

    @Override
    @Transactional
    public AuthTokens execute(String refreshToken) {
        String hashedRefreshToken = tokenProvider.hashToken(refreshToken);
        RefreshToken existingRefreshToken = refreshTokenRepository
                .getRefreshTokenByHashedToken(hashedRefreshToken)
                .orElseThrow(() -> new RefreshTokenUnknownException("Refresh token not found"));

        if (existingRefreshToken.revoked()) {
            throw new RefreshTokenRevokedException("Refresh token has been revoked");
        }

        if (existingRefreshToken.expiresAt().isBefore(LocalDateTime.now())) {
            throw new RefreshTokenExpiredException("Refresh token has expired");
        }

        SecureUserInfoApiDto user = userApi.getSecureUserDataById(existingRefreshToken.userId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        return new AuthTokens(
                tokenProvider.generateAccessToken(user.userId(), user.email()),
                refreshToken,
                user.isEmailVerified()
        );
    }
}
