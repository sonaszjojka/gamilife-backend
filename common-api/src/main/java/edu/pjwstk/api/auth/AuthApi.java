package edu.pjwstk.api.auth;

import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.ChangePasswordCommand;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.auth.dto.RotateUserTokensCommand;

import java.util.Optional;

public interface AuthApi {

    Optional<CurrentUserDto> getCurrentUser();

    String handleChangePassword(ChangePasswordCommand dto);

    AuthTokens rotateUserTokens(RotateUserTokensCommand rotateUserTokensCommand);
}