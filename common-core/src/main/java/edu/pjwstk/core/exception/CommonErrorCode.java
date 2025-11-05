package edu.pjwstk.core.exception;

public enum CommonErrorCode implements ErrorCode {
    VALIDATION_ERROR,
    USER_NOT_FOUND,
    USER_ALREADY_EXISTS,
    PASSWORD_RESET_FAILED,
    GROUP_NOT_FOUND,
    GROUP_MEMBER_NOT_FOUND,
    TASK_NOT_FOUND,
    GROUP_ADMIN_PRIVILEGES_REQUIRED,
    RESOURCE_OWNER_PRIVILEGES_REQUIRED,
    INTERNAL_SERVER_ERROR;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "CORE";
    }
}
