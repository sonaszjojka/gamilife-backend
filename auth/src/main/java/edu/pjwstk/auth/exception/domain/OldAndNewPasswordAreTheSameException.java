package edu.pjwstk.auth.exception.domain;

import edu.pjwstk.auth.exception.AuthErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class OldAndNewPasswordAreTheSameException extends DomainException {
    public OldAndNewPasswordAreTheSameException() {
        super(AuthErrorCode.OLD_AND_NEW_PASSWORD_ARE_SAME, "Old and new passwords cannot be the same");
    }
}
