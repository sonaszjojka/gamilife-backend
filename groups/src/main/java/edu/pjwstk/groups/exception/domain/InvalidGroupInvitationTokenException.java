package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class InvalidGroupInvitationTokenException extends DomainException {
    public InvalidGroupInvitationTokenException(String message) {
        super(GroupErrorCode.INVALID_GROUP_INVITATION_TOKEN, message);
    }
}
