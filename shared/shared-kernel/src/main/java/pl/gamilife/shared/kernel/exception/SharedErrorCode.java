package pl.gamilife.shared.kernel.exception;

public enum SharedErrorCode implements ErrorCode {
    USER_NOT_FOUND,
    USER_ALREADY_EXISTS,
    PASSWORD_RESET_FAILED,
    GROUP_NOT_FOUND,
    GROUP_MEMBER_NOT_FOUND,
    TASK_NOT_FOUND,
    GROUP_ADMIN_PRIVILEGES_REQUIRED,
    RESOURCE_OWNER_PRIVILEGES_REQUIRED,
    USER_HAS_NOT_ENOUGH_MONEY;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "SHARED";
    }
}
