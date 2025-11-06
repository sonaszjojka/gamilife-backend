package edu.pjwstk.common.authApi;

import edu.pjwstk.common.authApi.dto.AuthTokens;
import edu.pjwstk.common.authApi.dto.ChangePasswordCommand;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.authApi.dto.RotateUserTokensCommand;

import java.util.Optional;

public interface AuthApi {

    Optional<CurrentUserDto> getCurrentUser();

    String handleChangePassword(ChangePasswordCommand dto);

    AuthTokens rotateUserTokens(RotateUserTokensCommand rotateUserTokensCommand);
}