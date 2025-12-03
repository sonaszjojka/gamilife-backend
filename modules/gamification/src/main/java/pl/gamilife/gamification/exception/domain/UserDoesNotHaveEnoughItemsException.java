package pl.gamilife.gamification.exception.domain;

import pl.gamilife.gamification.exception.GamificationErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserDoesNotHaveEnoughItemsException extends DomainException {
    public UserDoesNotHaveEnoughItemsException(String message) {
        super(GamificationErrorCode.USER_DOES_NOT_HAVE_ENOUGH_ITEMS, message);
    }
}
