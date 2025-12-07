package pl.gamilife.gamification.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class UserDoesNotHaveEnoughItemsException extends DomainException {
    public UserDoesNotHaveEnoughItemsException(String message) {
        super(GamificationErrorCode.USER_DOES_NOT_HAVE_ENOUGH_ITEMS, message);
    }
}
