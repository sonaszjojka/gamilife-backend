package edu.pjwstk.user.usecase;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;

public interface RegisterNewUserUseCase {
    BasicUserInfoApiDto execute(RegisterUserApiDto dto);
}
