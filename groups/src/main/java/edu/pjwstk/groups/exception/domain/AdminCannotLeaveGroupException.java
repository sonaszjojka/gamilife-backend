package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class AdminCannotLeaveGroupException extends DomainException {
    public AdminCannotLeaveGroupException(String message) {
        super(GroupErrorCode.ADMIN_CANNOT_LEAVE_GROUP, message);
    }
}
