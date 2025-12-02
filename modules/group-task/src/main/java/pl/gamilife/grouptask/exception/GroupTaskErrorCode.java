package pl.gamilife.grouptask.exception;

import pl.gamilife.infrastructure.core.exception.ErrorCode;

public enum GroupTaskErrorCode implements ErrorCode {
    GROUP_TASK_MEMBER_NOT_FOUND,
    GROUP_TASK_NOT_FOUND;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "GROUP_TASK";
    }
}
