package edu.pjwstk.core.exception.common;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class UserNotFoundException extends DomainException {
    public UserNotFoundException(String message) {
        super(CommonErrorCode.USER_NOT_FOUND, message);
    }
}
