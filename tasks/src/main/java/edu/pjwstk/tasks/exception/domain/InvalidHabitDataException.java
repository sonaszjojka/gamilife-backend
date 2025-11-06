package edu.pjwstk.tasks.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.tasks.exception.TaskErrorCode;

public class InvalidHabitDataException extends DomainException {
    public InvalidHabitDataException(String message) {
        super(TaskErrorCode.INVALID_HABIT_DATA, message);
    }
}
