package edu.pjwstk.auth.usecase.refreshtoken;

import edu.pjwstk.api.auth.dto.AuthTokens;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface RefreshAccessTokenUseCase extends UseCase<RefreshAccessTokenCommand, AuthTokens> {
}
