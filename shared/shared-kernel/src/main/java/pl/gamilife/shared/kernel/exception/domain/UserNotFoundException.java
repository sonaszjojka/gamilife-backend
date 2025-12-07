package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.shared.kernel.exception.SharedErrorCode;

public class UserNotFoundException extends DomainException {
    public UserNotFoundException(String message) {
        super(SharedErrorCode.USER_NOT_FOUND, message);
    }
}
