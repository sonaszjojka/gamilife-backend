package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class InvitationStatusNotFoundException extends DomainException {
    public InvitationStatusNotFoundException(String message) {
        super(GroupErrorCode.INVITATION_STATUS_NOT_FOUND, message);
    }
}
