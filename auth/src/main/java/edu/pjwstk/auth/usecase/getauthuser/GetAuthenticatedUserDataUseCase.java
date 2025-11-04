package edu.pjwstk.auth.usecase.getauthuser;

import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.UseCase;

import java.util.Optional;

public interface GetAuthenticatedUserDataUseCase extends UseCase<GetAuthenticatedUserCommand, CurrentUserDto> {
}
