package edu.pjwstk.user.usecase;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;

public interface RegisterNewUserUseCase {
    BasicUserInfoApiDto execute(RegisterUserApiDto dto);
}
