package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class GroupInvitationExpiredException extends DomainException {
    public GroupInvitationExpiredException(String message) {
        super(GroupErrorCode.GROUP_INVITATION_EXPIRED, message);
    }
}
