package edu.pjwstk.auth.exceptions;

import edu.pjwstk.core.exception.ErrorCode;

public enum AuthErrorCode implements ErrorCode {
    INVALID_CREDENTIALS,
    REFRESH_TOKEN_EXPIRED,
    INVALID_REFRESH_TOKEN,
    REFRESH_TOKEN_REVOKED,
    LINKED_USER_NOT_FOUND,
    USER_ALREADY_EXISTS,
    PASSWORD_RESET_FAILED,
    OLD_AND_NEW_PASSWORD_ARE_SAME;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "AUTH";
    }
}
