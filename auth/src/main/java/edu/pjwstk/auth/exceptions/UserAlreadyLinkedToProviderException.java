package edu.pjwstk.auth.exceptions;

public class UserAlreadyLinkedToProviderException extends RuntimeException {
    public UserAlreadyLinkedToProviderException(String message)
    {
        super(message);
    }
}
