package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupRequestNotFoundException extends DomainException {
    public GroupRequestNotFoundException(String message) {
        super(GroupErrorCode.GROUP_REQUEST_NOT_FOUND, message);
    }
}
