package pl.gamilife.auth.domain.exception.domain;

import pl.gamilife.auth.domain.exception.AuthErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class OldAndNewPasswordAreTheSameException extends DomainException {
    public OldAndNewPasswordAreTheSameException() {
        super(AuthErrorCode.OLD_AND_NEW_PASSWORD_ARE_SAME, "Old and new passwords cannot be the same");
    }
}
