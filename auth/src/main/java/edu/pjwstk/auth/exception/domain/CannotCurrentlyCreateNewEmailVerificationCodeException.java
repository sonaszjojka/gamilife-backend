package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class CannotCurrentlyCreateNewEmailVerificationCodeException extends DomainException {
    public CannotCurrentlyCreateNewEmailVerificationCodeException(String message) {
        super(AuthErrorCode.EMAIL_VERIFICATION_CODE_TOO_MANY_REQUESTS, message);
    }
}
