package edu.pjwstk.user.usecase;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.user.dto.service.ChangeUserPasswordCommand;

public interface ChangeUserPasswordUseCase {
    AuthTokens execute(ChangeUserPasswordCommand dto);
}
