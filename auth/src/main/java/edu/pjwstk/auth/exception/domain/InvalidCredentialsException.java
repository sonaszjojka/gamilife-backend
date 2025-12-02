package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class InvalidCredentialsException extends DomainException {
    public InvalidCredentialsException(String message) {
        super(AuthErrorCode.INVALID_CREDENTIALS, message);
    }
}
