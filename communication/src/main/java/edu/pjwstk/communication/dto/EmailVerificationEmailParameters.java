package edu.pjwstk.communication.dto;

import edu.pjwstk.communication.enums.EmailType;

public record EmailVerificationEmailParameters(String verificationLink) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.EMAIL_VERIFICATION;
    }
}
