package pl.gamilife.pomodoro.domain.exception;

import pl.gamilife.shared.kernel.exception.ErrorCode;

public enum PomodoroErrorCode implements ErrorCode {
    POMODORO_ITEM_NOT_FOUND,
    INVALID_POMODORO_ITEM_DATA;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "POMODORO";
    }
}
