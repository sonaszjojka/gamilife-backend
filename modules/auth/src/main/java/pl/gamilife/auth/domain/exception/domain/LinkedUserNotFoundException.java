package pl.gamilife.auth.domain.exception.domain;

import pl.gamilife.auth.domain.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class LinkedUserNotFoundException extends DomainException {
    public LinkedUserNotFoundException(String message) {
        super(AuthErrorCode.LINKED_USER_NOT_FOUND, message);
    }
}
