package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.SharedErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupAdminPrivilegesRequiredException extends DomainException {
    public GroupAdminPrivilegesRequiredException(String message) {
        super(SharedErrorCode.GROUP_ADMIN_PRIVILEGES_REQUIRED, message);
    }
}
