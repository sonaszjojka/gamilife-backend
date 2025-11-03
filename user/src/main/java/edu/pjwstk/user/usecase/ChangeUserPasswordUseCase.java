package edu.pjwstk.user.usecase;

import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.user.dto.service.ChangeUserPasswordCommand;

public interface ChangeUserPasswordUseCase {
    AuthTokens execute(ChangeUserPasswordCommand dto);
}
