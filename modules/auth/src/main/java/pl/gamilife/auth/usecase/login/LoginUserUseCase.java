package pl.gamilife.auth.usecase.login;

import pl.gamilife.auth.usecase.common.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface LoginUserUseCase extends UseCase<LoginUserCommand, LoginUserResult> {
}
