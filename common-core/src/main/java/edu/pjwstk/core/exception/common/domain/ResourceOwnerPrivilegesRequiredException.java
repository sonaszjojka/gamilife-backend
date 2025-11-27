package edu.pjwstk.core.exception.common.domain;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class ResourceOwnerPrivilegesRequiredException extends DomainException {
    public ResourceOwnerPrivilegesRequiredException(String message) {
        super(CommonErrorCode.RESOURCE_OWNER_PRIVILEGES_REQUIRED, message);
    }
}
