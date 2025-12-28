package pl.gamilife.auth.application.usecase.registeruser;

import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, BasicUserDetails> {
}
