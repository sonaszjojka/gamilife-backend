package pl.gamilife.auth.usecase.registeruser;

import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface RegisterUserUseCase extends UseCase<RegisterUserCommand, BasicUserInfoApiDto> {
}
