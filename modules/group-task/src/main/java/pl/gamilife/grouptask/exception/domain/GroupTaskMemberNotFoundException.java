package pl.gamilife.grouptask.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.grouptask.exception.GroupTaskErrorCode;

public class GroupTaskMemberNotFoundException extends DomainException {
    public GroupTaskMemberNotFoundException(String message) {
        super(GroupTaskErrorCode.GROUP_TASK_MEMBER_NOT_FOUND, message);
    }
}
