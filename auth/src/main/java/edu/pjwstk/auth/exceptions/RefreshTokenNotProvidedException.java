package edu.pjwstk.auth.exceptions;

public class RefreshTokenNotProvidedException extends RuntimeException {
    public RefreshTokenNotProvidedException(String message) {
        super(message);
    }
}
