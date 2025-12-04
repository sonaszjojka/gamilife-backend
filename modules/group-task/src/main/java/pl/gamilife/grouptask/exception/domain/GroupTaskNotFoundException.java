package pl.gamilife.grouptask.exception.domain;

import pl.gamilife.grouptask.exception.GroupTaskErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class GroupTaskNotFoundException extends DomainException {
    public GroupTaskNotFoundException(String message) {
        super(GroupTaskErrorCode.GROUP_TASK_NOT_FOUND, message);
    }
}
