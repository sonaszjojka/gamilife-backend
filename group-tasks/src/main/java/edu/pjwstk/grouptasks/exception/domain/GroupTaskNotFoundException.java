package edu.pjwstk.grouptasks.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.grouptasks.exception.GroupTaskErrorCode;

public class GroupTaskNotFoundException extends DomainException {
    public GroupTaskNotFoundException(String message) {
        super(GroupTaskErrorCode.GROUP_TASK_NOT_FOUND, message);
    }
}
