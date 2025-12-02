package edu.pjwstk.user.usecase;

import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import pl.gamification.api.user.dto.RegisterUserApiDto;

public interface RegisterNewUserUseCase {
    BasicUserInfoApiDto execute(RegisterUserApiDto dto);
}
