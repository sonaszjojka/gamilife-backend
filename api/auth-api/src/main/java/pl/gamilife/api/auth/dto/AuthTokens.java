package pl.gamilife.api.auth.dto;

public record AuthTokens(
        String accessToken,
        String refreshToken
) {
}
