package edu.pjwstk.auth.usecase.registeruser;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import jakarta.validation.Valid;

public interface RegisterUserUseCase {
    BasicUserInfoApiDto execute(@Valid RegisterUserCommand registerUserCommand);
}
