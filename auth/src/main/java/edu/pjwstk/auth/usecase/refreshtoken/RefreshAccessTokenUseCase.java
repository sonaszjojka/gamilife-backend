package edu.pjwstk.auth.usecase.refreshtoken;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.core.UseCase;

public interface RefreshAccessTokenUseCase extends UseCase<RefreshAccessTokenCommand, AuthTokens> {
}
