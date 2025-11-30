package edu.pjwstk.communication.dto;

import edu.pjwstk.communication.enums.EmailType;

public record ForgotPasswordEmailParameters(String resetLink) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.FORGOT_PASSWORD;
    }
}
