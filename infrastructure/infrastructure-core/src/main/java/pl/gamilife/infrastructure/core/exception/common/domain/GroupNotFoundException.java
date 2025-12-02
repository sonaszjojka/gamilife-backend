package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class GroupNotFoundException extends DomainException {
    public GroupNotFoundException(String message) {
        super(CommonErrorCode.GROUP_NOT_FOUND, message);
    }
}
