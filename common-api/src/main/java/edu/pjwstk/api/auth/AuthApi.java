package edu.pjwstk.api.auth;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.ChangePasswordDto;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.auth.dto.RotateUserTokensDto;

import java.util.Optional;

public interface AuthApi {

    CurrentUserDto getCurrentUser();

    String handleChangePassword(ChangePasswordDto dto);

    AuthTokens rotateUserTokens(RotateUserTokensDto rotateUserTokensDto);
}