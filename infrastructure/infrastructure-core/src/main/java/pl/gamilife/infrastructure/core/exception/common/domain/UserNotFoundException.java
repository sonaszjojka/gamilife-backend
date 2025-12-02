package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserNotFoundException extends DomainException {
    public UserNotFoundException(String message) {
        super(CommonErrorCode.USER_NOT_FOUND, message);
    }
}
