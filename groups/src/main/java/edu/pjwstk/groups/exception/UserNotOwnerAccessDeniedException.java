package edu.pjwstk.groups.exception;

public class UserNotOwnerAccessDeniedException extends RuntimeException {
    public UserNotOwnerAccessDeniedException(String message) {
        super(message);
    }
}
