package edu.pjwstk.groups.exception;

public class UserJoinGroupAccessDeniedException extends RuntimeException {
    public UserJoinGroupAccessDeniedException(String message) {
        super(message);
    }
}
