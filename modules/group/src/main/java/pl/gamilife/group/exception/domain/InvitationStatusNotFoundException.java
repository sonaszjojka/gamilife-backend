package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class InvitationStatusNotFoundException extends DomainException {
    public InvitationStatusNotFoundException(String message) {
        super(GroupErrorCode.INVITATION_STATUS_NOT_FOUND, message);
    }
}
