package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class LinkedUserNotFoundException extends DomainException {
    public LinkedUserNotFoundException(String message) {
        super(AuthErrorCode.LINKED_USER_NOT_FOUND, message);
    }
}
