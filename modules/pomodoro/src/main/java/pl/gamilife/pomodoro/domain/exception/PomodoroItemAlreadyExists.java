package pl.gamilife.pomodoro.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class PomodoroItemAlreadyExists extends DomainException {
    public PomodoroItemAlreadyExists(String message) {
        super(PomodoroErrorCode.POMODORO_ITEM_ALREADY_EXISTS, message);
    }
}
