package pl.gamilife.auth.domain.exception.domain;

import pl.gamilife.auth.domain.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class CannotCurrentlyCreateNewEmailVerificationCodeException extends DomainException {
    public CannotCurrentlyCreateNewEmailVerificationCodeException(String message) {
        super(AuthErrorCode.EMAIL_VERIFICATION_CODE_TOO_MANY_REQUESTS, message);
    }
}
