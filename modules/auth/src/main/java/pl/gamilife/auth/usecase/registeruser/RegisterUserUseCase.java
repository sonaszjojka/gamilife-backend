package pl.gamilife.auth.usecase.registeruser;

import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, BasicUserInfoApiDto> {
}
