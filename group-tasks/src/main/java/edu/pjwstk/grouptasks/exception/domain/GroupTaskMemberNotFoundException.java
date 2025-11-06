package edu.pjwstk.grouptasks.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.grouptasks.exception.GroupTaskErrorCode;

public class GroupTaskMemberNotFoundException extends DomainException {
    public GroupTaskMemberNotFoundException(String message) {
        super(GroupTaskErrorCode.GROUP_TASK_MEMBER_NOT_FOUND, message);
    }
}
