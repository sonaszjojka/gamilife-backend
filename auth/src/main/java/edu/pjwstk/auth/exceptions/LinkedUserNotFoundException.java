package edu.pjwstk.auth.exceptions;

public class LinkedUserNotFoundException extends RuntimeException {
    public LinkedUserNotFoundException(String message) {
        super(message);
    }
}
