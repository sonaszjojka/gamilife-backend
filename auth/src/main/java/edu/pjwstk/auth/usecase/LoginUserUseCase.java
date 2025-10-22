package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.LoginUserDto;
import edu.pjwstk.common.authApi.dto.AuthTokens;

public interface LoginUserUseCase {
    AuthTokens execute(LoginUserDto loginUserDto);
}
