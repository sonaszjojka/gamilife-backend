package edu.pjwstk.groups.exception;

public class UserNotGroupAdministratorAccessDeniedException extends RuntimeException {
    public UserNotGroupAdministratorAccessDeniedException(String message) {
        super(message);
    }
}
