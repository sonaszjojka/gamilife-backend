package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserAlreadyLinkedToProviderException extends DomainException {
    public UserAlreadyLinkedToProviderException(String message) {
        super(AuthErrorCode.USER_ALREADY_LINKED_TO_PROVIDER, message);
    }
}
