package pl.gamilife.auth.usecase.getauthuser;

import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface GetAuthenticatedUserDataUseCase extends UseCase<GetAuthenticatedUserCommand, CurrentUserDto> {
}
