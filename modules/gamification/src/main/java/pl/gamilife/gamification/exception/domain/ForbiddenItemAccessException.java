package pl.gamilife.gamification.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.gamification.exception.GamificationErrorCode;

public class ForbiddenItemAccessException extends DomainException {
    public ForbiddenItemAccessException(String message) {
        super(GamificationErrorCode.FORBIDDEN_ITEM_ACCESS, message);
    }
}
