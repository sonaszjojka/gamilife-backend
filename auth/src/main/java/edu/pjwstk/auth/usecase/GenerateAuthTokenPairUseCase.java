package edu.pjwstk.auth.usecase;

import edu.pjwstk.api.auth.dto.AuthTokens;

import java.util.UUID;

@Deprecated
public interface GenerateAuthTokenPairUseCase {
    AuthTokens execute(UUID userId, String email, boolean isEmailVerified);
}
