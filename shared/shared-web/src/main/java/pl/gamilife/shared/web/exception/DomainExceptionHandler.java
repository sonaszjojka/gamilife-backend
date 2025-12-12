package pl.gamilife.shared.web.exception;

import org.springframework.core.annotation.Order;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.shared.kernel.exception.ErrorCode;

@Order(1)
@RestControllerAdvice
public class DomainExceptionHandler extends AbstractExceptionHandler {

    @ExceptionHandler(DomainException.class)
    public ErrorResponse handleDomainException(DomainException ex) {
        ErrorCode errorCode = ex.getErrorCode();
        ErrorResponse response = buildErrorResponseFor(errorCode, ex);

        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

}
