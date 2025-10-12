package edu.pjwstk.auth.exceptions;

public class RefreshTokenUnknownException extends RuntimeException {
    public RefreshTokenUnknownException(String message) {
        super(message);
    }
}
