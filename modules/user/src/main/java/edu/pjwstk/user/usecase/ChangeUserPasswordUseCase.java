package edu.pjwstk.user.usecase;

import pl.gamilife.api.auth.dto.AuthTokens;
import edu.pjwstk.user.dto.service.ChangeUserPasswordCommand;

public interface ChangeUserPasswordUseCase {
    AuthTokens execute(ChangeUserPasswordCommand dto);
}
