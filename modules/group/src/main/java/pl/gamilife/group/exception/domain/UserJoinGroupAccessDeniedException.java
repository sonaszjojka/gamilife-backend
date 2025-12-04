package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserJoinGroupAccessDeniedException extends DomainException {
    public UserJoinGroupAccessDeniedException(String message) {
        super(GroupErrorCode.USER_JOIN_GROUP_ACCESS_DENIED, message);
    }
}
