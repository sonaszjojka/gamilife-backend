package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class UserAlreadyMemberOfGroupException extends DomainException {
    public UserAlreadyMemberOfGroupException(String message) {
        super(GroupErrorCode.USER_ALREADY_MEMBER_OF_GROUP, message);
    }
}
