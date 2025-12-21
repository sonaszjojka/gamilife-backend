package pl.gamilife.auth.application.getauthuser;

import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GetAuthenticatedUserDataUseCase extends UseCase<GetAuthenticatedUserCommand, CurrentUserDto> {
}
