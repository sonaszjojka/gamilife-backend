package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupRequestStatusNotFoundException extends DomainException {
    public GroupRequestStatusNotFoundException(String message) {
        super(GroupErrorCode.GROUP_REQUEST_STATUS_NOT_FOUND, message);
    }
}
