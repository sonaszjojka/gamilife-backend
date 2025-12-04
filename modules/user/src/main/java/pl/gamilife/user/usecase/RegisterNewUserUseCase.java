package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.api.user.dto.RegisterUserApiDto;

public interface RegisterNewUserUseCase {
    BasicUserInfoApiDto execute(RegisterUserApiDto dto);
}
