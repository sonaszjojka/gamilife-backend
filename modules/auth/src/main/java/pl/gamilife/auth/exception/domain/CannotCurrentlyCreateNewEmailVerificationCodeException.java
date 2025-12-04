package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class CannotCurrentlyCreateNewEmailVerificationCodeException extends DomainException {
    public CannotCurrentlyCreateNewEmailVerificationCodeException(String message) {
        super(AuthErrorCode.EMAIL_VERIFICATION_CODE_TOO_MANY_REQUESTS, message);
    }
}
