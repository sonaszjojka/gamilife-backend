package pl.gamilife.communication.exception;

import pl.gamilife.shared.kernel.exception.ApplicationException;

public class EmailSendingException extends ApplicationException {
    public EmailSendingException(String message) {
        super(message);
    }
}
