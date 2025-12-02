package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class InvalidGroupInvitationTokenException extends DomainException {
    public InvalidGroupInvitationTokenException(String message) {
        super(GroupErrorCode.INVALID_GROUP_INVITATION_TOKEN, message);
    }
}
