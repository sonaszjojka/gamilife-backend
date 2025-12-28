package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.EmailType;

public record EmailVerificationEmailParameters(String verificationLink) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.EMAIL_VERIFICATION;
    }
}
