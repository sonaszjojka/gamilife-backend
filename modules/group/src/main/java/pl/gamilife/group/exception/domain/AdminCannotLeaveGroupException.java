package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class AdminCannotLeaveGroupException extends DomainException {
    public AdminCannotLeaveGroupException(String message) {
        super(GroupErrorCode.ADMIN_CANNOT_LEAVE_GROUP, message);
    }
}
