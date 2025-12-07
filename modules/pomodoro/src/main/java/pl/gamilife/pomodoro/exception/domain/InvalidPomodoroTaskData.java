package pl.gamilife.pomodoro.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.pomodoro.exception.PomodoroErrorCode;

public class InvalidPomodoroTaskData extends DomainException {
    public InvalidPomodoroTaskData(String message) {
        super(PomodoroErrorCode.INVALID_POMODORO_TASK_DATA, message);
    }
}
