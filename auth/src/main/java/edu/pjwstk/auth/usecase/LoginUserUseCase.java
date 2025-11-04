package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.command.LoginUserCommand;
import edu.pjwstk.auth.usecase.result.LoginUserResult;

public interface LoginUserUseCase {
    LoginUserResult execute(LoginUserCommand loginUserCommand);
}
