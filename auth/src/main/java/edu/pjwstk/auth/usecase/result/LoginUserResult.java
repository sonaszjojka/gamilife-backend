package edu.pjwstk.auth.usecase.result;

import edu.pjwstk.api.auth.dto.AuthTokens;

import java.util.UUID;

public record LoginUserResult(
        UUID userId,
        String email,
        String username,
        boolean isEmailVerified,
        AuthTokens authTokens
) {
}
