package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class GroupTypeNotFoundException extends DomainException {
    public GroupTypeNotFoundException(String message) {
        super(GroupErrorCode.GROUP_TYPE_NOT_FOUND, message);
    }
}
