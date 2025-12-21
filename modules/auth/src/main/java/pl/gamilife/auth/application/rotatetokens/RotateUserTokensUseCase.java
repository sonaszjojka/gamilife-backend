package pl.gamilife.auth.application.rotatetokens;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RotateUserTokensUseCase extends UseCase<RotateUserTokensCommand, AuthTokens> {
}
