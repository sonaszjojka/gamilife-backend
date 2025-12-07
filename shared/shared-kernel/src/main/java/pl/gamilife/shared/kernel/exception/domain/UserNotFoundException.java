package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.CoreErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class UserNotFoundException extends DomainException {
    public UserNotFoundException(String message) {
        super(CoreErrorCode.USER_NOT_FOUND, message);
    }
}
