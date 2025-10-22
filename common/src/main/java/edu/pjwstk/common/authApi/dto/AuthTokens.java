package edu.pjwstk.common.authApi.dto;

public record AuthTokens(
        String accessToken,
        String refreshToken,
        boolean isEmailVerified
) {
}
