package pl.gamilife.auth.usecase.rotatetokens;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface RotateUserTokensUseCase extends UseCase<RotateUserTokensCommand, AuthTokens> {
}
