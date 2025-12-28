package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupTypeNotFoundException extends DomainException {
    public GroupTypeNotFoundException(String message) {
        super(GroupErrorCode.GROUP_TYPE_NOT_FOUND, message);
    }
}
