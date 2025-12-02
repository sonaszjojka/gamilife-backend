package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class InvalidEmailVerificationCodeException extends DomainException {
    public InvalidEmailVerificationCodeException(String message) {
        super(AuthErrorCode.INVALID_EMAIL_VERIFICATION_CODE, message);
    }
}
