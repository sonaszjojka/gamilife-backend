package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.AuthTokens;

public interface RefreshAccessTokenUseCase {
    AuthTokens execute(String refreshToken);
}
