package edu.pjwstk.common.authApi;

import edu.pjwstk.common.authApi.dto.CurrentUserDto;

import java.util.Optional;

public interface AuthApi {
    Optional<CurrentUserDto> getCurrentUser();
}
