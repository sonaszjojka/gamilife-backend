package pl.gamilife.shared.web.exception;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import pl.gamilife.shared.kernel.exception.ErrorCode;

@Slf4j
@AllArgsConstructor(access = AccessLevel.PROTECTED)
public abstract class AbstractExceptionHandler {

    protected ErrorCodesRepository errorCodesRepository;

    protected void logWarning(String code, String key, String message) {
        log.warn("Exception occurred: code={}, key={}, message={}",
                code, key, message);
    }

    protected void logError(Exception exception) {
        log.error("Unexpected exception occurred.", exception);
    }

    protected ErrorResponse buildErrorResponseFor(ErrorCode errorCode) {
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

    protected ErrorResponse buildErrorResponseFor(ErrorCode errorCode, Exception exception) {
        ErrorCodesRepository.ErrorDefinition errorDefinition = errorCodesRepository.get(errorCode.getKey());

        ErrorResponse response = ErrorResponse.of(
                errorDefinition.getStatus(),
                "https://gamilife.pl/errors/"
                        + errorCode.getModule().toLowerCase().replace("_", "-") + "/"
                        + errorCode.getKey().toLowerCase().replace("_", "-"),
                errorDefinition.getTitle(),
                exception.getMessage(),
                "/api/error"
        );

        response.setCode(String.valueOf(errorDefinition.getCode()));

        return response;
    }

}
