package pl.gamilife.auth.exception.domain;

import pl.gamilife.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class EmailVerificationCodeExpiredException extends DomainException {
    public EmailVerificationCodeExpiredException(String message) {
        super(AuthErrorCode.EMAIL_VERIFICATION_CODE_EXPIRED, message);
    }
}
