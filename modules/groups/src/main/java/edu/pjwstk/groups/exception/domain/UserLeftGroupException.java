package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class UserLeftGroupException extends DomainException {
    public UserLeftGroupException(String message) {
        super(GroupErrorCode.USER_LEFT_GROUP, message);
    }
}
