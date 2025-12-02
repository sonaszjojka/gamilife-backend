package pl.gamilife.user.usecase;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.user.dto.service.ChangeUserPasswordCommand;

public interface ChangeUserPasswordUseCase {
    AuthTokens execute(ChangeUserPasswordCommand dto);
}
