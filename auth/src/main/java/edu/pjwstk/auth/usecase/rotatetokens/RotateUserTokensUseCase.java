package edu.pjwstk.auth.usecase.rotatetokens;

import edu.pjwstk.api.auth.dto.AuthTokens;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface RotateUserTokensUseCase extends UseCase<RotateUserTokensCommand, AuthTokens> {
}
