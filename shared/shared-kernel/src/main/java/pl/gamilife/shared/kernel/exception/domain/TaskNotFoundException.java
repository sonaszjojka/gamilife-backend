package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.shared.kernel.exception.SharedErrorCode;

public class TaskNotFoundException extends DomainException {
    public TaskNotFoundException(String message) {
        super(SharedErrorCode.TASK_NOT_FOUND, message);
    }
}
