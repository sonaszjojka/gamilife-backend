package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class CannotCurrentlyCreateNewForgotPasswordCodeException extends DomainException {
    public CannotCurrentlyCreateNewForgotPasswordCodeException(String message) {
        super(AuthErrorCode.FORGOT_PASSWORD_CODE_TOO_MANY_REQUESTS, message);
    }
}
