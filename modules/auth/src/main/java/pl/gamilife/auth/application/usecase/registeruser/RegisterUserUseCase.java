package pl.gamilife.auth.application.usecase.registeruser;

import pl.gamilife.auth.application.dto.LoginUserResult;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, LoginUserResult> {
}
