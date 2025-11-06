package edu.pjwstk.core.exception.common.application;

import edu.pjwstk.core.exception.ApplicationException;

public class EmailSendingException extends ApplicationException {
    public EmailSendingException(String message) {
        super(message);
    }
}
