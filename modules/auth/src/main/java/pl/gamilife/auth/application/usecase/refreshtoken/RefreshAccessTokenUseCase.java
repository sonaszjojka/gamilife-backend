package pl.gamilife.auth.application.usecase.refreshtoken;

import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RefreshAccessTokenUseCase extends UseCase<RefreshAccessTokenCommand, AuthTokens> {
}
