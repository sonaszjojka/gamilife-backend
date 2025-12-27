package pl.gamilife.app.exception;

import jakarta.persistence.OptimisticLockException;
import jakarta.validation.ConstraintViolationException;
import jakarta.validation.ValidationException;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.StaleStateException;
import org.springframework.core.annotation.Order;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
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
import org.springframework.web.servlet.resource.NoResourceFoundException;
import pl.gamilife.shared.kernel.exception.ErrorCode;
import pl.gamilife.shared.web.exception.AbstractExceptionHandler;
import pl.gamilife.shared.web.exception.ErrorResponse;

@Slf4j
@Order(2)
@RestControllerAdvice
public class GlobalExceptionHandler extends AbstractExceptionHandler {

    @ExceptionHandler(Exception.class)
    public ErrorResponse handleException(Exception ex) {
        logError(ex);
        return buildErrorResponseFor(OtherErrorCode.INTERNAL_SERVER_ERROR);
    }

    @ExceptionHandler({
            MethodArgumentNotValidException.class
    })
    public ErrorResponse handleValidationException(MethodArgumentNotValidException ex) {
        ErrorCode errorCode = OtherErrorCode.VALIDATION_ERROR;
        ErrorResponse response = buildErrorResponseFor(errorCode);

        ex.getBindingResult().getFieldErrors().forEach(error ->
                response.addValidationError(error.getField(), error.getDefaultMessage())
        );

        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(ConstraintViolationException.class)
    public ErrorResponse handleConstraintViolationException(ConstraintViolationException ex) {
        ErrorCode errorCode = OtherErrorCode.VALIDATION_ERROR;
        ErrorResponse response = buildErrorResponseFor(errorCode);

        ex.getConstraintViolations().forEach(cv ->
                response.addValidationError(
                        cv.getPropertyPath().toString(),
                        cv.getMessage()
                )
        );

        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(ValidationException.class)
    public ErrorResponse handleValidationException(ValidationException ex) {
        ErrorCode errorCode = OtherErrorCode.VALIDATION_ERROR;
        ErrorResponse response = buildErrorResponseFor(errorCode, ex);

        logWarning(response.getCode(), errorCode.getKey(), ex.getMessage());

        return response;
    }


    @ExceptionHandler(HttpMessageNotReadableException.class)
    public ErrorResponse handleHttpMessageNotReadable() {
        ErrorCode errorCode = OtherErrorCode.MALFORMED_REQUEST;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MissingServletRequestParameterException.class)
    public ErrorResponse handleMissingParameter() {
        ErrorCode errorCode = OtherErrorCode.MISSING_PARAMETER;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(HttpRequestMethodNotSupportedException.class)
    public ErrorResponse handleMethodNotSupported() {
        ErrorCode errorCode = OtherErrorCode.METHOD_NOT_ALLOWED;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(HttpMediaTypeNotSupportedException.class)
    public ErrorResponse handleMediaTypeNotSupported() {
        ErrorCode errorCode = OtherErrorCode.UNSUPPORTED_MEDIA_TYPE;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(AccessDeniedException.class)
    public ErrorResponse handleAccessDenied() {
        ErrorCode errorCode = OtherErrorCode.ACCESS_DENIED;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MethodArgumentTypeMismatchException.class)
    public ErrorResponse handleTypeMismatch() {
        ErrorCode errorCode = OtherErrorCode.TYPE_MISMATCH;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MissingServletRequestPartException.class)
    public ErrorResponse handleMissingRequestPart() {
        ErrorCode errorCode = OtherErrorCode.MISSING_REQUEST_BODY;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(MissingRequestCookieException.class)
    public ErrorResponse handleMissingRequestCookieException(MissingRequestCookieException ex) {

        ErrorCode errorCode = switch (ex.getCookieName()) {
            case "REFRESH-TOKEN" -> OtherErrorCode.MISSING_REFRESH_TOKEN_COOKIE;
            case "ACCESS-TOKEN" -> OtherErrorCode.MISSING_ACCESS_TOKEN_COOKIE;
            default -> OtherErrorCode.MISSING_REQUEST_COOKIE;
        };

        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler({
            OptimisticLockException.class,
            StaleStateException.class,
            ObjectOptimisticLockingFailureException.class
    })
    public ErrorResponse handleOptimisticLockException() {
        ErrorCode errorCode = OtherErrorCode.OPTIMISTIC_LOCKING_FAILURE;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

    @ExceptionHandler(NoResourceFoundException.class)
    public ErrorResponse handleNoResourceFoundException() {
        ErrorCode errorCode = OtherErrorCode.NO_RESOURCE_FOUND;
        ErrorResponse response = buildErrorResponseFor(errorCode);
        logWarning(response.getCode(), errorCode.getKey(), response.getDetail());

        return response;
    }

}
