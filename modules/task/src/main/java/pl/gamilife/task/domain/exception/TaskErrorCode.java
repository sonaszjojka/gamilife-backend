package pl.gamilife.task.domain.exception;

import pl.gamilife.shared.kernel.exception.ErrorCode;

public enum TaskErrorCode implements ErrorCode {
    HABIT_NOT_FOUND,
    INVALID_HABIT_DATA,
    INVALID_TASK_DATA,
    TASK_CATEGORY_NOT_FOUND,
    TASK_DIFFICULTY_NOT_FOUND;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "TASK";
    }
}
