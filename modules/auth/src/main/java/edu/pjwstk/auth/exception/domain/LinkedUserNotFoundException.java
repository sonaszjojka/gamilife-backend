package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class LinkedUserNotFoundException extends DomainException {
    public LinkedUserNotFoundException(String message) {
        super(AuthErrorCode.LINKED_USER_NOT_FOUND, message);
    }
}
