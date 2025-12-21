package pl.gamilife.auth.application.login;

import pl.gamilife.auth.application.common.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface LoginUserUseCase extends UseCase<LoginUserCommand, LoginUserResult> {
}
