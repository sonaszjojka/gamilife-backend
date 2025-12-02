package edu.pjwstk.gamification.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.gamification.exception.GamificationErrorCode;

public class UserDoesNotHaveEnoughItemsException extends DomainException {
    public UserDoesNotHaveEnoughItemsException(String message) {
        super(GamificationErrorCode.USER_DOES_NOT_HAVE_ENOUGH_ITEMS, message);
    }
}
