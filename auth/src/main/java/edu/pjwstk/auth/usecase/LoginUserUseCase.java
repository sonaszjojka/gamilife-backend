package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.auth.dto.service.LoginUserResult;
import edu.pjwstk.common.authApi.dto.AuthTokens;

public interface LoginUserUseCase {
    LoginUserResult execute(LoginUserDto loginUserDto);
}
