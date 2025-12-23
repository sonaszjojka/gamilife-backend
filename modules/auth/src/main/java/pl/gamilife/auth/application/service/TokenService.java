package pl.gamilife.auth.application.service;

import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.dto.TokenClaims;

import java.util.UUID;

public interface TokenService {
    String generateAccessToken(UUID userId, String email, boolean isEmailVerified);

    TokenClaims validateTokenAndExtractClaims(String token);

    AuthTokens generateTokenPair(UUID userId, String email, boolean isEmailVerified);

    String hashToken(String token);

    void revokeAllActiveRefreshTokensByUserId(UUID userId);
}
