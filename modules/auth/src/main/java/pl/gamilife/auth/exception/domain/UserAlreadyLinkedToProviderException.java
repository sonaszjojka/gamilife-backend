package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class UserAlreadyLinkedToProviderException extends DomainException {
    public UserAlreadyLinkedToProviderException(String message) {
        super(AuthErrorCode.USER_ALREADY_LINKED_TO_PROVIDER, message);
    }
}
