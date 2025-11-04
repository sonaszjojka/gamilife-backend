package edu.pjwstk.auth.service;

import edu.pjwstk.api.auth.dto.AuthTokens;
import io.jsonwebtoken.Claims;

import java.util.UUID;

public interface TokenService {
    String generateAccessToken(UUID userId, String email);

    Claims validateTokenAndExtractClaims(String token);

    AuthTokens generateTokenPair(UUID userId, String email, boolean isEmailVerified);

    String hashToken(String token);

    void revokeAllActiveRefreshTokensByUserId(UUID userId);
}
