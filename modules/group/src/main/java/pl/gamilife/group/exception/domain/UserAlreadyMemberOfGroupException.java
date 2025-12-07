package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class UserAlreadyMemberOfGroupException extends DomainException {
    public UserAlreadyMemberOfGroupException(String message) {
        super(GroupErrorCode.USER_ALREADY_MEMBER_OF_GROUP, message);
    }
}
