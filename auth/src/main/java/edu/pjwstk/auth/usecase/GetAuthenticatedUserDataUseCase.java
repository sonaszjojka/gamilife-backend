package edu.pjwstk.auth.usecase;

import edu.pjwstk.common.authApi.dto.CurrentUserDto;

import java.util.Optional;

public interface GetAuthenticatedUserDataUseCase {
    Optional<CurrentUserDto> execute();
}
