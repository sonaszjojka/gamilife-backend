package edu.pjwstk.core.exception.common;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class GroupAdminPrivilegesRequiredException extends DomainException {
    public GroupAdminPrivilegesRequiredException(String message) {
        super(CommonErrorCode.GROUP_ADMIN_PRIVILEGES_REQUIRED, message);
    }
}
