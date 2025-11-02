package edu.pjwstk.auth.util;

import edu.pjwstk.common.authApi.dto.AuthTokens;
import io.jsonwebtoken.Claims;

import java.util.UUID;

public interface TokenProvider {
    String generateAccessToken(UUID userId, String email);

    Claims validateTokenAndExtractClaims(String token);

    AuthTokens generateTokenPair(UUID userId, String email, boolean isEmailVerified);

    String hashToken(String token);

    long getRefreshTokenExpirationTime();
}
