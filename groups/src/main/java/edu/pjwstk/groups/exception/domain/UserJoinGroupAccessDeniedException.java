package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class UserJoinGroupAccessDeniedException extends DomainException {
    public UserJoinGroupAccessDeniedException(String message) {
        super(GroupErrorCode.USER_JOIN_GROUP_ACCESS_DENIED, message);
    }
}
