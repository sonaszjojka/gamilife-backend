package pl.gamilife.auth.application.common;

import pl.gamilife.api.auth.dto.AuthTokens;

import java.util.UUID;

public record LoginUserResult(
        UUID userId,
        String email,
        String username,
        boolean isEmailVerified,
        boolean isTutorialCompleted,
        int money,
        AuthTokens authTokens
) {
}
