package pl.gamilife.auth.service;

import io.jsonwebtoken.Claims;
import pl.gamilife.api.auth.dto.AuthTokens;

import java.util.UUID;

public interface TokenService {
    String generateAccessToken(UUID userId, String email, boolean isEmailVerified);

    Claims validateTokenAndExtractClaims(String token);

    AuthTokens generateTokenPair(UUID userId, String email, boolean isEmailVerified);

    String hashToken(String token);

    void revokeAllActiveRefreshTokensByUserId(UUID userId);
}
