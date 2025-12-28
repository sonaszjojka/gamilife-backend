package pl.gamilife.gamification.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class ForbiddenItemAccessException extends DomainException {
    public ForbiddenItemAccessException(String message) {
        super(GamificationErrorCode.FORBIDDEN_ITEM_ACCESS, message);
    }
}
