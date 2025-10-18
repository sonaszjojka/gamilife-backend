package edu.pjwstk.auth.dto.service;

public record AuthTokens(
        String accessToken,
        String refreshToken,
        boolean isEmailVerified
) {
}
