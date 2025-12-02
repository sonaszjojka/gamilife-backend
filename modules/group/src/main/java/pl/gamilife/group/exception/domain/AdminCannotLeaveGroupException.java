package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class AdminCannotLeaveGroupException extends DomainException {
    public AdminCannotLeaveGroupException(String message) {
        super(GroupErrorCode.ADMIN_CANNOT_LEAVE_GROUP, message);
    }
}
