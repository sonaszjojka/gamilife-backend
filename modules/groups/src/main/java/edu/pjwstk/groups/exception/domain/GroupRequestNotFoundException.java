package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupRequestNotFoundException extends DomainException {
    public GroupRequestNotFoundException(String message) {
        super(GroupErrorCode.GROUP_REQUEST_NOT_FOUND, message);
    }
}
