package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.CoreErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class ResetPasswordGenericException extends DomainException {
    public ResetPasswordGenericException() {
        super(CoreErrorCode.PASSWORD_RESET_FAILED, "An error occurred while resetting the password.");
    }
}
