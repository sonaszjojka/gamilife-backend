package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class InvalidGroupDataException extends DomainException {
    public InvalidGroupDataException(String message) {
        super(GroupErrorCode.INVALID_GROUP_DATA, message);
    }
}
