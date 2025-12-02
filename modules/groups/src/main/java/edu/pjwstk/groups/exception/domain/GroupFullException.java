package edu.pjwstk.groups.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groups.exception.GroupErrorCode;

public class GroupFullException extends DomainException {
    public GroupFullException(String message) {
        super(GroupErrorCode.GROUP_FULL, message);
    }
}
