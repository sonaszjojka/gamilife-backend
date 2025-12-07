package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.CoreErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupMemberNotFoundException extends DomainException {
    public GroupMemberNotFoundException(String message) {
        super(CoreErrorCode.GROUP_MEMBER_NOT_FOUND, message);
    }
}
