package pl.gamilife.gamification.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.gamification.exception.GamificationErrorCode;

public class ItemNotFoundException extends DomainException {
    public ItemNotFoundException(String message) {
        super(GamificationErrorCode.ITEM_NOT_FOUND, message);
    }
}
