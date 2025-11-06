package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class InvalidGroupDataException extends DomainException {
    public InvalidGroupDataException(String message) {
        super(GroupErrorCode.INVALID_GROUP_DATA, message);
    }
}
