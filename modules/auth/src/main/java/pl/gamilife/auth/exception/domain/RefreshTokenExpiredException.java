package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class RefreshTokenExpiredException extends DomainException {
    public RefreshTokenExpiredException(String message) {
        super(AuthErrorCode.REFRESH_TOKEN_EXPIRED, message);
    }
}
