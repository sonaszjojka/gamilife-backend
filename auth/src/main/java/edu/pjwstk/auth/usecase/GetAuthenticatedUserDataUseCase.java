package edu.pjwstk.auth.usecase;

import edu.pjwstk.api.auth.dto.CurrentUserDto;

import java.util.Optional;

public interface GetAuthenticatedUserDataUseCase {
    Optional<CurrentUserDto> execute();
}
