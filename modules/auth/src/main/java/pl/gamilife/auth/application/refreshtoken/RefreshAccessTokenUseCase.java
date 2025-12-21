package pl.gamilife.auth.application.refreshtoken;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RefreshAccessTokenUseCase extends UseCase<RefreshAccessTokenCommand, AuthTokens> {
}
