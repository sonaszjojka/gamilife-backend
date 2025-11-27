package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class CannotCurrentlyCreateNewForgotPasswordCodeException extends DomainException {
    public CannotCurrentlyCreateNewForgotPasswordCodeException(String message) {
        super(AuthErrorCode.FORGOT_PASSWORD_CODE_TOO_MANY_REQUESTS, message);
    }
}
