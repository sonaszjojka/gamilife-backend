package pl.gamilife.task.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.task.exception.TaskErrorCode;

public class TaskCategoryNotFoundException extends DomainException {
    public TaskCategoryNotFoundException(String message) {
        super(TaskErrorCode.TASK_CATEGORY_NOT_FOUND, message);
    }
}
