package pl.gamilife.pomodoro.exception.domain;

import pl.gamilife.pomodoro.exception.PomodoroErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidPomodoroTaskData extends DomainException {
    public InvalidPomodoroTaskData(String message) {
        super(PomodoroErrorCode.INVALID_POMODORO_TASK_DATA, message);
    }
}
