package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.EmailType;

public record ForgotPasswordEmailParameters(String resetLink) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.FORGOT_PASSWORD;
    }
}
