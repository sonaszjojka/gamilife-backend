package pl.gamilife.auth.application.usecase.changepassword;

import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface ChangePasswordUseCase extends UseCase<ChangePasswordCommand, AuthTokens> {
}
