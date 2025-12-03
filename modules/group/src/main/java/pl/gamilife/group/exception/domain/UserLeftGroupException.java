package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserLeftGroupException extends DomainException {
    public UserLeftGroupException(String message) {
        super(GroupErrorCode.USER_LEFT_GROUP, message);
    }
}
