package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class GroupMemberNotFoundException extends DomainException {
    public GroupMemberNotFoundException(String message) {
        super(CommonErrorCode.GROUP_MEMBER_NOT_FOUND, message);
    }
}
