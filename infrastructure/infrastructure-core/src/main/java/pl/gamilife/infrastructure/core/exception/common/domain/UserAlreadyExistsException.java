package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserAlreadyExistsException extends DomainException {
    public UserAlreadyExistsException(String message) {
        super(CommonErrorCode.USER_ALREADY_EXISTS, message);
    }
}
