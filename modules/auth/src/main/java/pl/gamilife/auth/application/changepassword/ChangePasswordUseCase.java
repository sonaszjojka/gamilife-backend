package pl.gamilife.auth.application.changepassword;

import pl.gamilife.auth.application.AuthTokens;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface ChangePasswordUseCase extends UseCase<ChangePasswordCommand, AuthTokens> {
}
