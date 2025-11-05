package edu.pjwstk.api.auth.exception;

public class ResetPasswordGenericException extends RuntimeException {
    public ResetPasswordGenericException() {
        super("Reset Password Failed");
    }
}
