package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupInvitationNotFoundException extends DomainException {
    public GroupInvitationNotFoundException(String message) {
        super(GroupErrorCode.GROUP_INVITATION_NOT_FOUND, message);
    }
}
