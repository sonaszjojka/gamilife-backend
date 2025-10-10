package edu.pjwstk.pomodoro.exception;

public class InvalidPomodoroTaskData extends RuntimeException {
    public InvalidPomodoroTaskData(String message) {
        super(message);
    }
}
