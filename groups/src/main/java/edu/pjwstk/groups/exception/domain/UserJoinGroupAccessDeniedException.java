package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class UserJoinGroupAccessDeniedException extends DomainException {
    public UserJoinGroupAccessDeniedException(String message) {
        super(GroupErrorCode.USER_JOIN_GROUP_ACCESS_DENIED, message);
    }
}
