package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.SharedErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class ResourceOwnerPrivilegesRequiredException extends DomainException {
    public ResourceOwnerPrivilegesRequiredException(String message) {
        super(SharedErrorCode.RESOURCE_OWNER_PRIVILEGES_REQUIRED, message);
    }
}
