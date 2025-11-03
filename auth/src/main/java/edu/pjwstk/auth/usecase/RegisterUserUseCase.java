package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import jakarta.validation.Valid;

public interface RegisterUserUseCase {
    BasicUserInfoApiDto execute(@Valid RegisterUserDto registerUserDto);
}
