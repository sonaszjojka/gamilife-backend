package edu.pjwstk.common.emailSenderApi;

public interface EmailSenderApi {
    void sendEmail(MailDto email) throws EmailSendingException;
}
