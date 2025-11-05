package edu.pjwstk.api.emailSender;

import edu.pjwstk.core.exception.common.EmailSendingException;

public interface EmailSenderApi {
    void sendEmail(MailDto email) throws EmailSendingException;
}
