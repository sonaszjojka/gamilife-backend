package edu.pjwstk.auth.usecase;

import edu.pjwstk.api.auth.dto.AuthTokens;

public interface RefreshAccessTokenUseCase {
    AuthTokens execute(String refreshToken);
}
