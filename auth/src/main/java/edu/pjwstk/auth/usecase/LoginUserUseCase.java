package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.LoginUserDto;

public interface LoginUserUseCase {
    AuthTokens execute(LoginUserDto loginUserDto);
}
