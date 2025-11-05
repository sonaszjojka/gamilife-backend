package edu.pjwstk.groupshop.exception;

public class UnauthorizedUserActionException extends RuntimeException {
    public UnauthorizedUserActionException(String message) {
        super(message);
    }
}
