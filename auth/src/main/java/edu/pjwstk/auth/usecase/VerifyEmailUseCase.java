package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.command.VerifyEmailCommand;
import edu.pjwstk.auth.usecase.result.LoginUserResult;

public interface VerifyEmailUseCase {
    LoginUserResult execute(VerifyEmailCommand code);
}
