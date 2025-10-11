package edu.pjwstk.auth.exceptions;

public class EmptyAccessTokenException extends RuntimeException {
    public EmptyAccessTokenException(String message) {
        super(message);
    }
}
