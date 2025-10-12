package edu.pjwstk.auth.util;

import edu.pjwstk.auth.dto.service.AuthTokens;
import org.springframework.security.core.Authentication;

import java.util.UUID;

public interface TokenProvider {
    String generateAccessToken(UUID userId, String email);

    AuthTokens generateTokenPair(UUID userId, String email);

    boolean validateToken(String token);

    Authentication getAuthenticationFromToken(String token);

    String hashToken(String token);

    long getRefreshTokenExpirationTime();
}
