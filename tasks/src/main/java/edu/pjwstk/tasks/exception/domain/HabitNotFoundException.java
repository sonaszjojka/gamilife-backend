package edu.pjwstk.tasks.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.tasks.exception.TaskErrorCode;

public class HabitNotFoundException extends DomainException {
    public HabitNotFoundException(String message) {
        super(TaskErrorCode.HABIT_NOT_FOUND, message);
    }
}
