package pl.gamilife.auth.usecase.refreshtoken;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface RefreshAccessTokenUseCase extends UseCase<RefreshAccessTokenCommand, AuthTokens> {
}
