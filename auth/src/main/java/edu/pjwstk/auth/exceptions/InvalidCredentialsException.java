package edu.pjwstk.auth.exceptions;

import edu.pjwstk.core.exception.DomainException;

public class InvalidCredentialsException extends DomainException {

    public InvalidCredentialsException() {
        super(AuthErrorCode.INVALID_CREDENTIALS, "Invalid Credentials");
    }

    public InvalidCredentialsException(String message) {
        super(AuthErrorCode.INVALID_CREDENTIALS, message);
    }
}
