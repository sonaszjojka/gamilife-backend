package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class UserAlreadyMemberOfGroupException extends DomainException {
    public UserAlreadyMemberOfGroupException(String message) {
        super(GroupErrorCode.USER_ALREADY_MEMBER_OF_GROUP, message);
    }
}
