package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class ResetPasswordGenericException extends DomainException {
    public ResetPasswordGenericException() {
        super(CommonErrorCode.PASSWORD_RESET_FAILED, "An error occurred while resetting the password.");
    }
}
