package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class GroupAdminPrivilegesRequiredException extends DomainException {
    public GroupAdminPrivilegesRequiredException(String message) {
        super(CommonErrorCode.GROUP_ADMIN_PRIVILEGES_REQUIRED, message);
    }
}
