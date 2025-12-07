package pl.gamilife.infrastructure.core.exception.domain;

import pl.gamilife.infrastructure.core.exception.CoreErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserNotFoundException extends DomainException {
    public UserNotFoundException(String message) {
        super(CoreErrorCode.USER_NOT_FOUND, message);
    }
}
