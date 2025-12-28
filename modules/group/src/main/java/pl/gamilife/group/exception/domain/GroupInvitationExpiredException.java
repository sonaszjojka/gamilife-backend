package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupInvitationExpiredException extends DomainException {
    public GroupInvitationExpiredException(String message) {
        super(GroupErrorCode.GROUP_INVITATION_EXPIRED, message);
    }
}
