package edu.pjwstk.auth.usecase;

import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.authApi.dto.RotateUserTokensCommand;

public interface RotateUserTokensUseCase {
    AuthTokens execute(RotateUserTokensCommand rotateUserTokensCommand);
}
