package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.DomainException;
import pl.gamilife.shared.kernel.exception.SharedErrorCode;

public class DomainValidationException extends DomainException {
    public DomainValidationException(String message) {
        super(SharedErrorCode.DOMAIN_VALIDATION_FAILED, message);
    }
}
