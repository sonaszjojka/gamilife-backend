package pl.gamilife.auth.application.dto;

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
