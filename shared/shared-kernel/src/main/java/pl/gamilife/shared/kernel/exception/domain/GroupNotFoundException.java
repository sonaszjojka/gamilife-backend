package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.SharedErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupNotFoundException extends DomainException {
    public GroupNotFoundException(String message) {
        super(SharedErrorCode.GROUP_NOT_FOUND, message);
    }
}
