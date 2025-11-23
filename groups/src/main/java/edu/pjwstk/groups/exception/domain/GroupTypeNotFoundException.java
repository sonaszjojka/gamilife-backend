package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupTypeNotFoundException extends DomainException {
    public GroupTypeNotFoundException(String message) {
        super(GroupErrorCode.GROUP_TYPE_NOT_FOUND, message);
    }
}
