package pl.gamilife.auth.application.registeruser;

import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, BasicUserDetails> {
}
