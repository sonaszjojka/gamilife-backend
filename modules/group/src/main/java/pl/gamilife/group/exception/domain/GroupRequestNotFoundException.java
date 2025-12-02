package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class GroupRequestNotFoundException extends DomainException {
    public GroupRequestNotFoundException(String message) {
        super(GroupErrorCode.GROUP_REQUEST_NOT_FOUND, message);
    }
}
