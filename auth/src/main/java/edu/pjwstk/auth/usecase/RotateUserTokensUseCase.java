package edu.pjwstk.auth.usecase;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.RotateUserTokensCommand;

public interface RotateUserTokensUseCase {
    AuthTokens execute(RotateUserTokensCommand rotateUserTokensCommand);
}
