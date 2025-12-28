package pl.gamilife.gamification.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class ItemIsNotForSaleException extends DomainException {
    public ItemIsNotForSaleException(String message) {
        super(GamificationErrorCode.ITEM_NOT_FOR_SALE, message);
    }
}
