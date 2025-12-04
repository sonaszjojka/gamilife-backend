package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class GroupFullException extends DomainException {
    public GroupFullException(String message) {
        super(GroupErrorCode.GROUP_FULL, message);
    }
}
