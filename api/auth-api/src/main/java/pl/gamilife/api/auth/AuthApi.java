package pl.gamilife.api.auth;

import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.auth.dto.ChangePasswordDto;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.auth.dto.RotateUserTokensDto;

public interface AuthApi {

    CurrentUserDto getCurrentUser();

    String handleChangePassword(ChangePasswordDto dto);

    AuthTokens rotateUserTokens(RotateUserTokensDto rotateUserTokensDto);
}