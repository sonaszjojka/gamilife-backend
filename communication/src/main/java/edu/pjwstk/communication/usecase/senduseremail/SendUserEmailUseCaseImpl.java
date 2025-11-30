package edu.pjwstk.communication.usecase.senduseremail;

import com.sendgrid.Method;
import com.sendgrid.Request;
import com.sendgrid.Response;
import com.sendgrid.SendGrid;
import com.sendgrid.helpers.mail.Mail;
import com.sendgrid.helpers.mail.objects.Content;
import com.sendgrid.helpers.mail.objects.Email;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.communication.dto.EmailContent;
import edu.pjwstk.communication.dto.EmailParameters;
import edu.pjwstk.communication.util.EmailContentCreator;
import edu.pjwstk.core.exception.common.application.EmailSendingException;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.UUID;

@Service
@Slf4j
@RequiredArgsConstructor
public class SendUserEmailUseCaseImpl implements SendUserEmailUseCase {

    private final UserApi userApi;
    private final EmailContentCreator emailContentCreator;

    @Value("${sendgrid.api-key}")
    private String apiKey;

    @Value("${sendgrid.from-email}")
    private String fromEmail;

    @Value("${sendgrid.from-name}")
    private String fromName;

    @Value("${sendgrid.send-emails}")
    private boolean sendEmails;

    @Override
    public Void execute(SendUserEmailCommand cmd) {
        if (!sendEmails) {
            log.info("Email sending is disabled - skipping");
            return null;
        }

        BasicUserInfoApiDto user = getUser(cmd.userId());
        Mail mail = getMail(cmd.emailParameters(), user.email());

        sendEmail(mail);

        return null;
    }

    private Mail getMail(EmailParameters emailParameters, String toEmail) {
        Email from = new Email(fromEmail, fromName);
        Email to = new Email(toEmail);
        EmailContent emailContent = emailContentCreator.createContentForParameters(emailParameters);
        String subject = emailContent.getSubject();
        Content content = new Content(
                emailContent.getContentType(),
                emailContent.getContent()
        );

        return new Mail(
                from,
                subject,
                to,
                content
        );
    }

    private BasicUserInfoApiDto getUser(UUID userId) {
        return userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }

    private void sendEmail(Mail mail) {
        SendGrid sg = new SendGrid(apiKey);
        try {
            Request request = new Request();
            request.setMethod(Method.POST);
            request.setEndpoint("mail/send");
            request.setBody(mail.build());
            Response response = sg.api(request);
            log.info("Email send response code: {}; body: {}", response.getStatusCode(), response.getBody());
        } catch (IOException ex) {
            throw new EmailSendingException("An error occurred when trying to send an email.");
        }
    }
}
