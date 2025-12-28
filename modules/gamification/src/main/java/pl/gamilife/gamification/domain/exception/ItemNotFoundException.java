package pl.gamilife.gamification.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class ItemNotFoundException extends DomainException {
    public ItemNotFoundException(String message) {
        super(GamificationErrorCode.ITEM_NOT_FOUND, message);
    }
}
