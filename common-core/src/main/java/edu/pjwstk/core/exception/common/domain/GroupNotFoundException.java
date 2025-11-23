package edu.pjwstk.core.exception.common.domain;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class GroupNotFoundException extends DomainException {
    public GroupNotFoundException(String message) {
        super(CommonErrorCode.GROUP_NOT_FOUND, message);
    }
}
