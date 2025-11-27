package edu.pjwstk.groups.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupInvitationNotFoundException extends DomainException {
    public GroupInvitationNotFoundException(String message) {
        super(GroupErrorCode.GROUP_INVITATION_NOT_FOUND, message);
    }
}
