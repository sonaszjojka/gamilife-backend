package edu.pjwstk.tasks.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.tasks.exception.TaskErrorCode;

public class TaskCategoryNotFoundException extends DomainException {
    public TaskCategoryNotFoundException(String message) {
        super(TaskErrorCode.TASK_CATEGORY_NOT_FOUND, message);
    }
}
