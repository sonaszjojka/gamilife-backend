package pl.gamilife.auth.usecase.common;

import pl.gamilife.api.auth.dto.AuthTokens;

import java.util.UUID;

public record LoginUserResult(
        UUID userId,
        String email,
        String username,
        boolean isEmailVerified,
        boolean isTutorialCompleted,
        AuthTokens authTokens
) {
}
