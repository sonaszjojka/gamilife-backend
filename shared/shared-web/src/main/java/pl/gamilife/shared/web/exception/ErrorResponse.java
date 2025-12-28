package pl.gamilife.shared.web.exception;

import lombok.Getter;
import lombok.Setter;
import org.springframework.http.ProblemDetail;

import java.net.URI;
import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

@Getter
@Setter
public class ErrorResponse extends ProblemDetail {
    private String code;
    private Instant timestamp;
    private Map<String, String> fieldErrors = new HashMap<>();

    private ErrorResponse() {
        super();
    }

    public static ErrorResponse of(int status) {
        ErrorResponse error = new ErrorResponse();
        error.setStatus(status);
        error.setTimestamp(Instant.now());

        return error;
    }

    public static ErrorResponse of(
            int status,
            String type,
            String title,
            String detail,
            String instance) {
        ErrorResponse error = new ErrorResponse();
        error.setStatus(status);
        error.setType(URI.create(type));
        error.setTitle(title);
        error.setDetail(detail);
        error.setInstance(URI.create(instance));
        error.setTimestamp(Instant.now());

        return error;
    }

    public void addValidationError(String fieldName, String message) {
        fieldErrors.put(fieldName, message);
    }
}
