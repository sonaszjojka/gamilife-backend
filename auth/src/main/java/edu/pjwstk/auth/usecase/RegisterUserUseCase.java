package edu.pjwstk.auth.usecase;

import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.auth.usecase.command.RegisterUserCommand;
import jakarta.validation.Valid;

public interface RegisterUserUseCase {
    BasicUserInfoApiDto execute(@Valid RegisterUserCommand registerUserCommand);
}
