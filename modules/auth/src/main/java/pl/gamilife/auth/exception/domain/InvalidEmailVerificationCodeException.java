package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidEmailVerificationCodeException extends DomainException {
    public InvalidEmailVerificationCodeException(String message) {
        super(AuthErrorCode.INVALID_EMAIL_VERIFICATION_CODE, message);
    }
}
