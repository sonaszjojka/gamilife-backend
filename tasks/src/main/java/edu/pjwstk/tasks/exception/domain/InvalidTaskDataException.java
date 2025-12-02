package edu.pjwstk.tasks.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.tasks.exception.TaskErrorCode;

public class InvalidTaskDataException extends DomainException {
    public InvalidTaskDataException(String message) {
        super(TaskErrorCode.INVALID_TASK_DATA, message);
    }
}
