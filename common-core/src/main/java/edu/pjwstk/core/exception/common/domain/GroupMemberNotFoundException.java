package edu.pjwstk.core.exception.common.domain;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class GroupMemberNotFoundException extends DomainException {
    public GroupMemberNotFoundException(String message) {
        super(CommonErrorCode.GROUP_MEMBER_NOT_FOUND, message);
    }
}
