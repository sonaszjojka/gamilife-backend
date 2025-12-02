package edu.pjwstk.tasks.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.tasks.exception.TaskErrorCode;

public class InvalidHabitDataException extends DomainException {
    public InvalidHabitDataException(String message) {
        super(TaskErrorCode.INVALID_HABIT_DATA, message);
    }
}
