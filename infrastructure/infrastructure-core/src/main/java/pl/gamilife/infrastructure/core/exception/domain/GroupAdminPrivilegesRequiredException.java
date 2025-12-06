package pl.gamilife.infrastructure.core.exception.domain;

import pl.gamilife.infrastructure.core.exception.CoreErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class GroupAdminPrivilegesRequiredException extends DomainException {
    public GroupAdminPrivilegesRequiredException(String message) {
        super(CoreErrorCode.GROUP_ADMIN_PRIVILEGES_REQUIRED, message);
    }
}
