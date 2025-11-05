package edu.pjwstk.auth.usecase.rotatetokens;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.core.UseCase;

public interface RotateUserTokensUseCase extends UseCase<RotateUserTokensCommand, AuthTokens> {
}
