package edu.pjwstk.tasks.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.tasks.exception.TaskErrorCode;

public class HabitNotFoundException extends DomainException {
    public HabitNotFoundException(String message) {
        super(TaskErrorCode.HABIT_NOT_FOUND, message);
    }
}
