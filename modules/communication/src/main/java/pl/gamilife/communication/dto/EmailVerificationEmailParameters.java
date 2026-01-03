package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.EmailType;

import java.util.Map;

public record EmailVerificationEmailParameters(String verificationCode) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.EMAIL_VERIFICATION;
    }

    @Override
    public Map<String, String> getParametersMap() {
        return Map.of(
                "verificationCode", verificationCode
        );
    }
}
