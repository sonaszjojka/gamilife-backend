package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class InvalidGroupInvitationDataException extends DomainException {
    public InvalidGroupInvitationDataException(String message) {
        super(GroupErrorCode.INVALID_GROUP_INVITATION_DATA, message);
    }
}
