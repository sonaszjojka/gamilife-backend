package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;

public interface RegisterUserUseCase {
    BasicUserInfoApiDto execute(RegisterUserDto registerUserDto);
}
