package edu.pjwstk.core.exception.common;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class TaskNotFoundException extends DomainException {
    public TaskNotFoundException(String message) {
        super(CommonErrorCode.TASK_NOT_FOUND, message);
    }
}
