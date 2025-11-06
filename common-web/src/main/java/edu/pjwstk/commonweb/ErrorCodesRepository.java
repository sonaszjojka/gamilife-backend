package edu.pjwstk.commonweb;

import lombok.Data;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

@Slf4j
@Configuration
@ConfigurationProperties(prefix = "error-codes")
@Data
public class ErrorCodesRepository {

    private Map<String, ErrorDefinition> definitions = new HashMap<>();

    public ErrorDefinition get(String errorKey) {
        ErrorDefinition definition = definitions.get(errorKey);

        if (definition == null) {
            log.error("Error definition not found for key: '{}'. Available keys: {}",
                    errorKey, definitions.keySet());
            throw new IllegalStateException(
                    String.format("Error definition not found for key: '%s'. " +
                            "Please add it to error-codes/*.yml", errorKey)
            );
        }

        return definition;
    }

    @Getter
    @Setter
    public static class ErrorDefinition {
        private String code;
        private String title;
        private String detail;
        private int status;
    }
}
