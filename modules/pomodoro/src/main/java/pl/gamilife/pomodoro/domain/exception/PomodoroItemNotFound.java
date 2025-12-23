package pl.gamilife.pomodoro.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class PomodoroItemNotFound extends DomainException {
    public PomodoroItemNotFound(String message) {
        super(PomodoroErrorCode.POMODORO_ITEM_NOT_FOUND, message);
    }
}
