package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.EmailType;

import java.util.Map;

public record ForgotPasswordEmailParameters(String forgotPasswordCode) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.FORGOT_PASSWORD;
    }

    @Override
    public Map<String, String> getParametersMap() {
        return Map.of(
                "forgotPasswordCode", forgotPasswordCode
        );
    }
}
