package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class RefreshTokenExpiredException extends DomainException {
    public RefreshTokenExpiredException(String message) {
        super(AuthErrorCode.REFRESH_TOKEN_EXPIRED, message);
    }
}
