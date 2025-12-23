package pl.gamilife.auth.application.dto;

public record AuthTokens(
        String accessToken,
        String refreshToken
) {
}
