package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class UserLeftGroupException extends DomainException {
    public UserLeftGroupException(String message) {
        super(GroupErrorCode.USER_LEFT_GROUP, message);
    }
}
