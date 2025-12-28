package pl.gamilife.auth.application.usecase.login;

import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface LoginUserUseCase extends UseCase<LoginUserCommand, LoginUserResult> {
}
