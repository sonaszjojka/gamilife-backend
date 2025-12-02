package edu.pjwstk.communication.exception;

import pl.gamilife.infrastructure.core.exception.ApplicationException;

public class EmailSendingException extends ApplicationException {
    public EmailSendingException(String message) {
        super(message);
    }
}
