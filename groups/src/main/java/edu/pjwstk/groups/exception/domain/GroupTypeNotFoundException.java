package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupTypeNotFoundException extends DomainException {
    public GroupTypeNotFoundException(String message) {
        super(GroupErrorCode.GROUP_TYPE_NOT_FOUND, message);
    }
}
