package pl.gamilife.infrastructure.core.exception.domain;

import pl.gamilife.infrastructure.core.exception.CoreErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class ResourceOwnerPrivilegesRequiredException extends DomainException {
    public ResourceOwnerPrivilegesRequiredException(String message) {
        super(CoreErrorCode.RESOURCE_OWNER_PRIVILEGES_REQUIRED, message);
    }
}
