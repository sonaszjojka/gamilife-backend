package edu.pjwstk.auth.dto.service;

import edu.pjwstk.common.authApi.dto.AuthTokens;

import java.util.UUID;

public record LoginUserResult(
        UUID userId,
        String email,
        String username,
        boolean isEmailVerified,
        AuthTokens authTokens
) {
}
