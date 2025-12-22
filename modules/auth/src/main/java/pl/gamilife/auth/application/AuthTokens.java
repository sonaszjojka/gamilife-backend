package pl.gamilife.auth.application;

public record AuthTokens(
        String accessToken,
        String refreshToken
) {
}
