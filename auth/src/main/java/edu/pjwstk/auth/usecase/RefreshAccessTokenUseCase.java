package edu.pjwstk.auth.usecase;

import edu.pjwstk.common.authApi.dto.AuthTokens;

public interface RefreshAccessTokenUseCase {
    AuthTokens execute(String refreshToken);
}
