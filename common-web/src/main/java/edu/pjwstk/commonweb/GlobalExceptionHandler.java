package edu.pjwstk.commonweb;

import edu.pjwstk.core.exception.DomainException;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@Slf4j
@AllArgsConstructor
@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(DomainException.class)
    public ErrorResponse handleDomainException(DomainException ex) {
        return ErrorResponse.of(
                400,
                "https://example.com/probs/" + ex.getErrorCode().getModule() + "/" + ex.getErrorCode().getKey(),
                "Domain Error",
                ex.getMessage(),
                "/domain-error"
        );
    }

}
