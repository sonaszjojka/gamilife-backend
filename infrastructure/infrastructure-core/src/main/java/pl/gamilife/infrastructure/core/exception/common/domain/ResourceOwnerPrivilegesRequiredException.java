package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class ResourceOwnerPrivilegesRequiredException extends DomainException {
    public ResourceOwnerPrivilegesRequiredException(String message) {
        super(CommonErrorCode.RESOURCE_OWNER_PRIVILEGES_REQUIRED, message);
    }
}
