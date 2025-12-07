package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.CoreErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupNotFoundException extends DomainException {
    public GroupNotFoundException(String message) {
        super(CoreErrorCode.GROUP_NOT_FOUND, message);
    }
}
