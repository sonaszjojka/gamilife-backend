package edu.pjwstk.api.emailSender;

public interface EmailSenderApi {
    void sendEmail(MailDto email) throws EmailSendingException;
}
