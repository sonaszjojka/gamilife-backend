package pl.gamilife.auth.application.usecase.registeruser;

import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, AuthTokens> {
}
