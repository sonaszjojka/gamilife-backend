package edu.pjwstk.pomodoro.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.pomodoro.exception.PomodoroErrorCode;

public class InvalidPomodoroTaskData extends DomainException {
    public InvalidPomodoroTaskData(String message) {
        super(PomodoroErrorCode.INVALID_POMODORO_TASK_DATA, message);
    }
}
