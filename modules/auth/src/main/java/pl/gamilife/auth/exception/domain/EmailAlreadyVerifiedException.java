package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class EmailAlreadyVerifiedException extends DomainException {
    public EmailAlreadyVerifiedException(String message) {
        super(AuthErrorCode.EMAIL_ALREADY_VERIFIED, message);
    }
}
