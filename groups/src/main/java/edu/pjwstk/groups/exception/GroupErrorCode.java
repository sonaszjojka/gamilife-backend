package edu.pjwstk.groups.exception;

import edu.pjwstk.core.exception.ErrorCode;

public enum GroupErrorCode implements ErrorCode {
    ADMIN_CANNOT_LEAVE_GROUP,
    GROUP_FULL,
    GROUP_INVITATION_EXPIRED,
    GROUP_INVITATION_NOT_FOUND,
    GROUP_REQUEST_NOT_FOUND,
    GROUP_REQUEST_STATUS_NOT_FOUND,
    GROUP_TYPE_NOT_FOUND,
    INVALID_GROUP_DATA,
    INVALID_GROUP_INVITATION_DATA,
    INVALID_GROUP_INVITATION_TOKEN,
    INVITATION_STATUS_NOT_FOUND,
    USER_ALREADY_MEMBER_OF_GROUP,
    USER_JOIN_GROUP_ACCESS_DENIED,
    USER_LEFT_GROUP;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "GROUP";
    }
}
