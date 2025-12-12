package pl.gamilife.pomodoro.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class PomodoroTaskNotFound extends DomainException {
    public PomodoroTaskNotFound(String message) {
        super(PomodoroErrorCode.POMODORO_TASK_NOT_FOUND, message);
    }
}
