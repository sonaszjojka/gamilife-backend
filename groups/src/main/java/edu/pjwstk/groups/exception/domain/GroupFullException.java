package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupFullException extends DomainException {
    public GroupFullException(String message) {
        super(GroupErrorCode.GROUP_FULL, message);
    }
}
