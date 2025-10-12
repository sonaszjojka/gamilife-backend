package edu.pjwstk.pomodoro.exception;

public class PomodoroTaskNotFound extends RuntimeException {
    public PomodoroTaskNotFound(String message) {
        super(message);
    }
}
