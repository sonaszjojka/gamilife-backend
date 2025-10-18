package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.EmailVerificationCode;

public interface VerifyEmailUseCase {
    AuthTokens execute(EmailVerificationCode code);
}
