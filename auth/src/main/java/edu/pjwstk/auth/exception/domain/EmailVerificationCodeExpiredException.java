package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class EmailVerificationCodeExpiredException extends DomainException {
    public EmailVerificationCodeExpiredException(String message) {
        super(AuthErrorCode.EMAIL_VERIFICATION_CODE_EXPIRED, message);
    }
}
