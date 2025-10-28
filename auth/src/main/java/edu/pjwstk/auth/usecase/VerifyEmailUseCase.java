package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.EmailVerificationCode;
import edu.pjwstk.auth.dto.service.LoginUserResult;
import edu.pjwstk.common.authApi.dto.AuthTokens;

public interface VerifyEmailUseCase {
    LoginUserResult execute(EmailVerificationCode code);
}
