package edu.pjwstk.groups.exception;

public class UserAlreadyMemberOfGroupException extends RuntimeException {
    public UserAlreadyMemberOfGroupException(String message) {
        super(message);
    }
}
