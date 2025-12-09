package pl.gamilife.task.domain.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.task.domain.exception.TaskErrorCode;

public class HabitNotFoundException extends DomainException {
    public HabitNotFoundException(String message) {
        super(TaskErrorCode.HABIT_NOT_FOUND, message);
    }
}
