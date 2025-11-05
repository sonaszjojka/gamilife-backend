package edu.pjwstk.pomodoro.exception;

import edu.pjwstk.core.exception.ErrorCode;

public enum PomodoroErrorCode implements ErrorCode {
    POMODORO_TASK_NOT_FOUND,
    INVALID_POMODORO_TASK_DATA;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "POMODORO";
    }
}
