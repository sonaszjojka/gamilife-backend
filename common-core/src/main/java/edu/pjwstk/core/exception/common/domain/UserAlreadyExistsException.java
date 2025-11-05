package edu.pjwstk.core.exception.common.domain;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class UserAlreadyExistsException extends DomainException {
    public UserAlreadyExistsException(String message) {
        super(CommonErrorCode.USER_ALREADY_EXISTS, message);
    }
}
