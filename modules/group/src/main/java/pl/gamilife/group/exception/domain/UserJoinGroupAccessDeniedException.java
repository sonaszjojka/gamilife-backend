package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class UserJoinGroupAccessDeniedException extends DomainException {
    public UserJoinGroupAccessDeniedException(String message) {
        super(GroupErrorCode.USER_JOIN_GROUP_ACCESS_DENIED, message);
    }
}
