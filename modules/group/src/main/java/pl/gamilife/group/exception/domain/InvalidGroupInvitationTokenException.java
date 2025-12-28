package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidGroupInvitationTokenException extends DomainException {
    public InvalidGroupInvitationTokenException(String message) {
        super(GroupErrorCode.INVALID_GROUP_INVITATION_TOKEN, message);
    }
}
