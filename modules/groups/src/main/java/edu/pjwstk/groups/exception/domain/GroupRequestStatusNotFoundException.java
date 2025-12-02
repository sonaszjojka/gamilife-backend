package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupRequestStatusNotFoundException extends DomainException {
    public GroupRequestStatusNotFoundException(String message) {
        super(GroupErrorCode.GROUP_REQUEST_STATUS_NOT_FOUND, message);
    }
}
