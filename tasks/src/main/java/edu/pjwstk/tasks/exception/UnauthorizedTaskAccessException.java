package edu.pjwstk.tasks.exception;

public class UnauthorizedTaskAccessException extends RuntimeException {
    public UnauthorizedTaskAccessException(String message) {
        super(message);
    }
}
