package pl.gamilife.infrastructure.core.exception.domain;

import pl.gamilife.infrastructure.core.exception.CoreErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class GroupNotFoundException extends DomainException {
    public GroupNotFoundException(String message) {
        super(CoreErrorCode.GROUP_NOT_FOUND, message);
    }
}
