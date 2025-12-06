package pl.gamilife.gamification.domain.exception;

import pl.gamilife.infrastructure.core.exception.DomainException;

public class ForbiddenItemAccessException extends DomainException {
    public ForbiddenItemAccessException(String message) {
        super(GamificationErrorCode.FORBIDDEN_ITEM_ACCESS, message);
    }
}
