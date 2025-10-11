package edu.pjwstk.auth.exceptions;

public class UnsupportedAccessTokenException extends RuntimeException {
    public UnsupportedAccessTokenException(String message) {
        super(message);
    }
}
