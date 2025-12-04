package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class TaskNotFoundException extends DomainException {
    public TaskNotFoundException(String message) {
        super(CommonErrorCode.TASK_NOT_FOUND, message);
    }
}
