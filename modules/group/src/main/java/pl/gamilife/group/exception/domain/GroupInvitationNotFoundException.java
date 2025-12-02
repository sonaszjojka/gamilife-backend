package pl.gamilife.group.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.group.exception.GroupErrorCode;

public class GroupInvitationNotFoundException extends DomainException {
    public GroupInvitationNotFoundException(String message) {
        super(GroupErrorCode.GROUP_INVITATION_NOT_FOUND, message);
    }
}
