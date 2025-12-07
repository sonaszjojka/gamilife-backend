package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.shared.kernel.exception.SharedErrorCode;

public class GroupMemberNotFoundException extends DomainException {
    public GroupMemberNotFoundException(String message) {
        super(SharedErrorCode.GROUP_MEMBER_NOT_FOUND, message);
    }
}
