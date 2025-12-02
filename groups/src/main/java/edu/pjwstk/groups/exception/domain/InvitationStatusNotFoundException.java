package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class InvitationStatusNotFoundException extends DomainException {
    public InvitationStatusNotFoundException(String message) {
        super(GroupErrorCode.INVITATION_STATUS_NOT_FOUND, message);
    }
}
