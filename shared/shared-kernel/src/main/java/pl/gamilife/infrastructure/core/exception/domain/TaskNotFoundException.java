package pl.gamilife.infrastructure.core.exception.domain;

import pl.gamilife.infrastructure.core.exception.CoreErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class TaskNotFoundException extends DomainException {
    public TaskNotFoundException(String message) {
        super(CoreErrorCode.TASK_NOT_FOUND, message);
    }
}
