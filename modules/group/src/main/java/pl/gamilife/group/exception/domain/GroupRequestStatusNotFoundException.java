package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupRequestStatusNotFoundException extends DomainException {
    public GroupRequestStatusNotFoundException(String message) {
        super(GroupErrorCode.GROUP_REQUEST_STATUS_NOT_FOUND, message);
    }
}
