package edu.pjwstk.application.configuration;

import edu.pjwstk.EmailSenderApiImpl;
import edu.pjwstk.api.emailSender.EmailSenderApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class EmailSenderConfiguration {

    @Bean
    public EmailSenderApi emailSenderApi(
            @Value("${sendgrid.api-key}")
            String sendGridApiKey,
            @Value("${sendgrid.from-email}")
            String sendGridFromEmail,
            @Value("${sendgrid.from-name}")
            String sendGridFromName,
            @Value("${sendgrid.send-emails}")
            boolean sendGridSendEmails
    ) {
        return new EmailSenderApiImpl(sendGridApiKey, sendGridFromEmail, sendGridFromName, sendGridSendEmails);
    }
}
