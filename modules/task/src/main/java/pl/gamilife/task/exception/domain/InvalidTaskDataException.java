package pl.gamilife.task.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.task.exception.TaskErrorCode;

public class InvalidTaskDataException extends DomainException {
    public InvalidTaskDataException(String message) {
        super(TaskErrorCode.INVALID_TASK_DATA, message);
    }
}
