package edu.pjwstk.common.authApi.exception;

public class ResetPasswordGenericException extends RuntimeException {
    public ResetPasswordGenericException() {
        super("Reset Password Failed");
    }
}
