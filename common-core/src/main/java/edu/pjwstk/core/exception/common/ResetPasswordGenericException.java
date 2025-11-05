package edu.pjwstk.core.exception.common;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class ResetPasswordGenericException extends DomainException {
    public ResetPasswordGenericException() {
        super(CommonErrorCode.PASSWORD_RESET_FAILED, "An error occurred while resetting the password.");
    }
}
