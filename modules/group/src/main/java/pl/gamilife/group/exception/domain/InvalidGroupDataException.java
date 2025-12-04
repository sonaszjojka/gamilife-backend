package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class InvalidGroupDataException extends DomainException {
    public InvalidGroupDataException(String message) {
        super(GroupErrorCode.INVALID_GROUP_DATA, message);
    }
}
