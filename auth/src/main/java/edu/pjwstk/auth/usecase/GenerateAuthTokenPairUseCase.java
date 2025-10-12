package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.AuthTokens;

import java.util.UUID;

public interface GenerateAuthTokenPairUseCase {
    AuthTokens execute(UUID userId, String email);
}
