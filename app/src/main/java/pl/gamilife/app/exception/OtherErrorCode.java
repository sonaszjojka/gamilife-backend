package pl.gamilife.app.exception;

import pl.gamilife.shared.kernel.exception.ErrorCode;

public enum OtherErrorCode implements ErrorCode {
    VALIDATION_ERROR,
    INTERNAL_SERVER_ERROR,
    MALFORMED_REQUEST,
    MISSING_PARAMETER,
    METHOD_NOT_ALLOWED,
    UNSUPPORTED_MEDIA_TYPE,
    ACCESS_DENIED,
    TYPE_MISMATCH,
    MISSING_REQUEST_BODY,
    MISSING_REQUEST_COOKIE,
    MISSING_ACCESS_TOKEN_COOKIE,
    MISSING_REFRESH_TOKEN_COOKIE,
    OPTIMISTIC_LOCKING_FAILURE;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "OTHER";
    }
}
