package edu.pjwstk.api.emailSender;

import edu.pjwstk.core.exception.common.application.EmailSendingException;

public interface EmailSenderApi {
    void sendEmail(MailDto email) throws EmailSendingException;
}
