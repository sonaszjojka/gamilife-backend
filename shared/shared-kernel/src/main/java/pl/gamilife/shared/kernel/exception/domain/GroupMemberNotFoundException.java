package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.SharedErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupMemberNotFoundException extends DomainException {
    public GroupMemberNotFoundException(String message) {
        super(SharedErrorCode.GROUP_MEMBER_NOT_FOUND, message);
    }
}
