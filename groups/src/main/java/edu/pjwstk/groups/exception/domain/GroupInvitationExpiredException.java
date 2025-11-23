package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupInvitationExpiredException extends DomainException {
    public GroupInvitationExpiredException(String message) {
        super(GroupErrorCode.GROUP_INVITATION_EXPIRED, message);
    }
}
