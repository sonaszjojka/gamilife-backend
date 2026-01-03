package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.EmailType;

import java.util.Map;

public interface EmailParameters {
    EmailType getEmailType();

    Map<String, String> getParametersMap();
}
