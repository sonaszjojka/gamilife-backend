package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.SharedErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class ResetPasswordGenericException extends DomainException {
    public ResetPasswordGenericException() {
        super(SharedErrorCode.PASSWORD_RESET_FAILED, "An error occurred while resetting the password.");
    }
}
