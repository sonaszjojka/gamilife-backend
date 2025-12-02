package pl.gamilife.infrastructure.web.exception;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingRequestCookieException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.multipart.support.MissingServletRequestPartException;
import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.infrastructure.core.exception.ErrorCode;

@Slf4j
@AllArgsConstructor
@RestControllerAdvice
public class GlobalExceptionHandler {

    private ErrorCodesRepository errorCodesRepository;

    @ExceptionHandler(DomainException.class)
    public ErrorResponse handleDomainException(DomainException ex) {
        ErrorCode errorCode = ex.getErrorCode();
        ErrorResponse response = buildErrorResponseFor(errorCode);

        log.warn("Domain exception occurred: code={}, key={}, message={}",
                response.getCode(), ex.getErrorCode().getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(Exception.class)
    public ErrorResponse handleException(Exception ex) {
        ErrorResponse response = buildErrorResponseFor(CommonErrorCode.INTERNAL_SERVER_ERROR);

        log.error("Unexpected exception occurred.", ex);

        return response;
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ErrorResponse handleValidationException(MethodArgumentNotValidException ex) {
        ErrorCode errorCode = CommonErrorCode.VALIDATION_ERROR;
        ErrorResponse response = buildErrorResponseFor(errorCode);

        ex.getBindingResult().getFieldErrors().forEach(error ->
                response.addValidationError(error.getField(), error.getDefaultMessage())
        );

        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ErrorResponse handleHttpMessageNotReadable() {
        ErrorCode errorCode = CommonErrorCode.MALFORMED_REQUEST;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MissingServletRequestParameterException.class)
    public ErrorResponse handleMissingParameter() {
        ErrorCode errorCode = CommonErrorCode.MISSING_PARAMETER;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    public ErrorResponse handleMethodNotSupported() {
        ErrorCode errorCode = CommonErrorCode.METHOD_NOT_ALLOWED;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(HttpMediaTypeNotSupportedException.class)
    public ErrorResponse handleMediaTypeNotSupported() {
        ErrorCode errorCode = CommonErrorCode.UNSUPPORTED_MEDIA_TYPE;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ErrorResponse handleAccessDenied() {
        ErrorCode errorCode = CommonErrorCode.ACCESS_DENIED;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ErrorResponse handleTypeMismatch() {
        ErrorCode errorCode = CommonErrorCode.TYPE_MISMATCH;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MissingServletRequestPartException.class)
    public ErrorResponse handleMissingRequestPart() {
        ErrorCode errorCode = CommonErrorCode.MISSING_REQUEST_BODY;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MissingRequestCookieException.class)
    public ErrorResponse handleMissingRequestCookieException(MissingRequestCookieException ex) {

        ErrorCode errorCode = switch(ex.getCookieName()){
            case "REFRESH-TOKEN" -> CommonErrorCode.MISSING_REFRESH_TOKEN_COOKIE;
            case "ACCESS-TOKEN" -> CommonErrorCode.MISSING_ACCESS_TOKEN_COOKIE;
            default -> CommonErrorCode.MISSING_REQUEST_COOKIE;
        };

        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }


    private void logWarning(String code, String key, String message) {
        log.warn("Exception occurred: code={}, key={}, message={}",
                code, key, message);
    }

    private ErrorResponse buildErrorResponseFor(ErrorCode errorCode) {
        ErrorCodesRepository.ErrorDefinition errorDefinition = errorCodesRepository.get(errorCode.getKey());

        ErrorResponse response = ErrorResponse.of(
                errorDefinition.getStatus(),
                "https://gamilife.pl/errors/"
                        + errorCode.getModule().toLowerCase().replace("_", "-") + "/"
                        + errorCode.getKey().toLowerCase().replace("_", "-"),
                errorDefinition.getTitle(),
                errorDefinition.getDetail(),
                "/api/error"
        );

        response.setCode(String.valueOf(errorDefinition.getCode()));

        return response;
    }

}
