package edu.pjwstk.auth.exceptions;

import edu.pjwstk.core.exception.ErrorCode;

public enum AuthErrorCode implements ErrorCode {
    INVALID_CREDENTIALS,
    REFRESH_TOKEN_EXPIRED,
    INVALID_REFRESH_TOKEN,
    REFRESH_TOKEN_REVOKED,
    LINKED_USER_NOT_FOUND,
    OLD_AND_NEW_PASSWORD_ARE_SAME,
    ACCESS_DENIED,
    REFRESH_TOKEN_UNKNOWN,
    REFRESH_TOKEN_NOT_PROVIDED,
    EMAIL_VERIFICATION_CODE_TOO_MANY_REQUESTS,
    PASSWORD_RESET_CODE_TOO_MANY_REQUESTS,
    EMAIL_ALREADY_VERIFIED,
    INVALID_EMAIL_VERIFICATION_CODE,
    EMAIL_VERIFICATION_CODE_EXPIRED;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "AUTH";
    }
}
