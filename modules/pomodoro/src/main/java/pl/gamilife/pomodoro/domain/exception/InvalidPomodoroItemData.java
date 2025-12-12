package pl.gamilife.pomodoro.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidPomodoroItemData extends DomainException {
    public InvalidPomodoroItemData(String message) {
        super(PomodoroErrorCode.INVALID_POMODORO_ITEM_DATA, message);
    }
}
