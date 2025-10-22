package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.EmailVerificationCode;
import edu.pjwstk.common.authApi.dto.AuthTokens;

public interface VerifyEmailUseCase {
    AuthTokens execute(EmailVerificationCode code);
}
