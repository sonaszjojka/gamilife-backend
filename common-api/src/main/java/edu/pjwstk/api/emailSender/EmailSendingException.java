package edu.pjwstk.api.emailSender;

import edu.pjwstk.core.exception.ApplicationException;

public class EmailSendingException extends ApplicationException {
    public EmailSendingException(String message) {
        super(message);
    }
}
