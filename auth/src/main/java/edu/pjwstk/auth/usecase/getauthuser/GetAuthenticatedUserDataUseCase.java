package edu.pjwstk.auth.usecase.getauthuser;

import edu.pjwstk.api.auth.dto.CurrentUserDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface GetAuthenticatedUserDataUseCase extends UseCase<GetAuthenticatedUserCommand, CurrentUserDto> {
}
