package pl.gamilife.auth.domain.exception.domain;

import pl.gamilife.auth.domain.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidRefreshTokenException extends DomainException {
    public InvalidRefreshTokenException(String message) {
        super(AuthErrorCode.INVALID_REFRESH_TOKEN, message);
    }
}
