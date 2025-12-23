package pl.gamilife.api.auth;

import pl.gamilife.api.auth.dto.CurrentUserDto;

public interface AuthApi {
    CurrentUserDto getCurrentUser();
}