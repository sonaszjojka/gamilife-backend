package edu.pjwstk.auth.exceptions;

public class CannotCurrentlyCreateNewEmailVerificationCodeException extends RuntimeException {
    public CannotCurrentlyCreateNewEmailVerificationCodeException(String message) {
        super(message);
    }
}
