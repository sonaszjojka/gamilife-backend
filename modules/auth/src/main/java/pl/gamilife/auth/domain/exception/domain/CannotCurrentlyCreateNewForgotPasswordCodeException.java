package pl.gamilife.auth.domain.exception.domain;

import pl.gamilife.auth.domain.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class CannotCurrentlyCreateNewForgotPasswordCodeException extends DomainException {
    public CannotCurrentlyCreateNewForgotPasswordCodeException(String message) {
        super(AuthErrorCode.FORGOT_PASSWORD_CODE_TOO_MANY_REQUESTS, message);
    }
}
