package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.SharedErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class UserAlreadyExistsException extends DomainException {
    public UserAlreadyExistsException(String message) {
        super(SharedErrorCode.USER_ALREADY_EXISTS, message);
    }
}
