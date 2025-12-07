package pl.gamilife.infrastructure.core.exception.domain;

import pl.gamilife.infrastructure.core.exception.CoreErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class ResetPasswordGenericException extends DomainException {
    public ResetPasswordGenericException() {
        super(CoreErrorCode.PASSWORD_RESET_FAILED, "An error occurred while resetting the password.");
    }
}
