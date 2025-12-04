package pl.gamilife.auth.usecase.login;

import pl.gamilife.auth.usecase.common.LoginUserResult;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface LoginUserUseCase extends UseCase<LoginUserCommand, LoginUserResult> {
}
