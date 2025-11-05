package edu.pjwstk;

import com.sendgrid.Method;
import com.sendgrid.Request;
import com.sendgrid.Response;
import com.sendgrid.SendGrid;
import com.sendgrid.helpers.mail.Mail;
import com.sendgrid.helpers.mail.objects.Content;
import com.sendgrid.helpers.mail.objects.Email;
import edu.pjwstk.api.emailSender.EmailSenderApi;
import edu.pjwstk.core.exception.common.EmailSendingException;
import edu.pjwstk.api.emailSender.MailDto;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;

@Slf4j
@AllArgsConstructor
public class EmailSenderApiImpl implements EmailSenderApi {

    private final String apiKey;
    private final String fromEmail;
    private final String fromName;
    private final boolean sendEmails;

    @Override
    public void sendEmail(@Valid MailDto email) throws EmailSendingException {
        if (!sendEmails) {
            log.info("Email sending is disabled - skipping");
            return;
        }

        Email from = new Email(fromEmail, fromName);
        Email to = new Email(email.toEmail());
        String subject = email.subject();
        Content content = new Content(
                email.mailContentType().getContentType(),
                email.content()
        );

        Mail mail = new Mail(
                from,
                subject,
                to,
                content
        );

        SendGrid sg = new SendGrid(apiKey);
        Request request = new Request();
        try {
            request.setMethod(Method.POST);
            request.setEndpoint("mail/send");
            request.setBody(mail.build());
            Response response = sg.api(request);
            log.info("Email send response code: {}", response.getStatusCode());
            log.info("Email send response body: {}", response.getBody());
        } catch (IOException ex) {
            throw new EmailSendingException("An error occurred when trying to send an email.");
        }
    }
}
