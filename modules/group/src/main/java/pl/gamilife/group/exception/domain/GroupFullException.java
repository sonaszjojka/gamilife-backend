package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class GroupFullException extends DomainException {
    public GroupFullException(String message) {
        super(GroupErrorCode.GROUP_FULL, message);
    }
}
