package edu.pjwstk.application.exception;

import io.swagger.v3.oas.annotations.media.Schema;
import org.springframework.http.HttpStatus;
import org.springframework.http.ProblemDetail;

import java.net.URI;

@Schema(
        name = "ProblemDetailDto",
        description = "DTO for ProblemDetail with additional error code",
        example = """
                {
                    "type": "https://inz-api.com/errors/1001",
                    "title": "Validation Error",
                    "detail": "The request contains invalid data.",
                    "status": 400,
                    "errorCode": 1001
                }
                """
)
public class ProblemDetailDto extends ProblemDetail {
    public ProblemDetailDto(HttpStatus status, String title, int errorCode) {
        super();
        this.setStatus(status);
        this.setTitle(title);
        this.setProperty("errorCode", errorCode);
        this.setType(URI.create("https://inz-api.com/errors/" + errorCode));
    }
}