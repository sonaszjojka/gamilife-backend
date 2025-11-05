package edu.pjwstk.auth.exception;

import edu.pjwstk.core.exception.ErrorCode;

public enum AuthErrorCode implements ErrorCode {
    INVALID_CREDENTIALS,
    REFRESH_TOKEN_EXPIRED,
    INVALID_REFRESH_TOKEN,
    LINKED_USER_NOT_FOUND,
    OLD_AND_NEW_PASSWORD_ARE_SAME,
    ACCESS_DENIED,
    EMAIL_VERIFICATION_CODE_TOO_MANY_REQUESTS,
    FORGOT_PASSWORD_CODE_TOO_MANY_REQUESTS,
    EMAIL_ALREADY_VERIFIED,
    INVALID_EMAIL_VERIFICATION_CODE,
    EMAIL_VERIFICATION_CODE_EXPIRED,
    USER_ALREADY_LINKED_TO_PROVIDER,
    UNAUTHORIZED;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "AUTH";
    }
}
