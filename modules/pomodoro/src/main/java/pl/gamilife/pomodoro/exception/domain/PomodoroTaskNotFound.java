package pl.gamilife.pomodoro.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.pomodoro.exception.PomodoroErrorCode;

public class PomodoroTaskNotFound extends DomainException {
    public PomodoroTaskNotFound(String message) {
        super(PomodoroErrorCode.POMODORO_TASK_NOT_FOUND, message);
    }
}
