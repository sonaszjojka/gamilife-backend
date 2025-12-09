package pl.gamilife.task.domain.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.task.domain.exception.TaskErrorCode;

public class InvalidHabitDataException extends DomainException {
    public InvalidHabitDataException(String message) {
        super(TaskErrorCode.INVALID_HABIT_DATA, message);
    }
}
