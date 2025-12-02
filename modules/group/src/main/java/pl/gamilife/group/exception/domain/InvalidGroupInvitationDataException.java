package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class InvalidGroupInvitationDataException extends DomainException {
    public InvalidGroupInvitationDataException(String message) {
        super(GroupErrorCode.INVALID_GROUP_INVITATION_DATA, message);
    }
}
