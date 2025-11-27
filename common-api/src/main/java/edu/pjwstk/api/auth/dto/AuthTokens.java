package edu.pjwstk.api.auth.dto;

public record AuthTokens(
        String accessToken,
        String refreshToken
) {
}
