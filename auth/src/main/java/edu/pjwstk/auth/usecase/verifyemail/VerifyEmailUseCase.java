package edu.pjwstk.auth.usecase.verifyemail;

import edu.pjwstk.auth.usecase.login.LoginUserResult;

public interface VerifyEmailUseCase {
    LoginUserResult execute(VerifyEmailCommand code);
}
