package edu.pjwstk.groupshop.exception;

public class UserNotAdministratorException extends RuntimeException {
    public UserNotAdministratorException(String message) {
        super(message);
    }
}
