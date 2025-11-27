package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class InvalidRefreshTokenException extends DomainException {
    public InvalidRefreshTokenException(String message) {
        super(AuthErrorCode.INVALID_REFRESH_TOKEN, message);
    }
}
