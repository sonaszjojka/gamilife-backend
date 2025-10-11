package edu.pjwstk.auth.util;

import org.springframework.security.core.Authentication;

import java.util.UUID;

public interface TokenProvider {
    String generateToken(UUID userId, String email);

    boolean validateToken(String token);

    Authentication getAuthentication(String token);

    String generateRefreshToken();

    String hashToken(String token);

    long getRefreshTokenExpirationTime();
}
