package edu.pjwstk.auth.exceptions;

public class OldAndNewPasswordAreTheSameException extends RuntimeException {
    public OldAndNewPasswordAreTheSameException() {
        super("Old and new passwords cannot be the same");
    }
}
