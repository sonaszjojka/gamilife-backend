package edu.pjwstk.auth.usecase.getauthuser;

import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.UseCase;

public interface GetAuthenticatedUserDataUseCase extends UseCase<GetAuthenticatedUserCommand, CurrentUserDto> {
}
