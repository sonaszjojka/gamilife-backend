package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.api.user.dto.RegisterUserDto;

public interface RegisterNewUserUseCase {
    BasicUserInfoDto execute(RegisterUserDto dto);
}
