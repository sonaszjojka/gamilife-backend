package edu.pjwstk.commonweb;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.core.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

@Slf4j
@AllArgsConstructor
@RestControllerAdvice
public class GlobalExceptionHandler {

    private ErrorCodesRepository errorCodesRepository;

    @ExceptionHandler(DomainException.class)
    public ErrorResponse handleDomainException(DomainException ex) {
        ErrorCode errorCode = ex.getErrorCode();
        ErrorCodesRepository.ErrorDefinition errorDefinition = errorCodesRepository.get(errorCode.getKey());

        ErrorResponse response = ErrorResponse.of(
                errorDefinition.getStatus(),
                "https://gamilife.pl/errors/" + errorCode.getModule() + "/" + errorCode.getKey(),
                errorDefinition.getTitle(),
                errorDefinition.getDetail(),
                "/api/error"
        );

        response.setCode(String.valueOf(errorDefinition.getCode()));

        log.warn("Domain exception occurred: code={}, key={}, message={}",
                errorDefinition.getCode(), errorCode.getKey(), errorDefinition.getDetail());

        return response;
    }

    @ExceptionHandler(Exception.class)
    public ErrorResponse handleException(Exception ex) {
        ErrorCode errorCode = CommonErrorCode.INTERNAL_SERVER_ERROR;

        ErrorCodesRepository.ErrorDefinition errorDefinition = errorCodesRepository.get(errorCode.getKey());

        ErrorResponse response = ErrorResponse.of(
                errorDefinition.getStatus(),
                "https://gamilife.pl/errors/" + errorCode.getModule() + "/" + errorCode.getKey(),
                errorDefinition.getTitle(),
                errorDefinition.getDetail(),
                "/api/error"
        );


        log.error("Internal server error occurred", ex);

        response.setCode(String.valueOf(errorDefinition.getCode()));

        return response;
    }


}
