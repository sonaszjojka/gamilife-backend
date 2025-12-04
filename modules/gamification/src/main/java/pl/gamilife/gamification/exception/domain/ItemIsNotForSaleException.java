package pl.gamilife.gamification.exception.domain;

import pl.gamilife.gamification.exception.GamificationErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class ItemIsNotForSaleException extends DomainException {
    public ItemIsNotForSaleException(String message) {
        super(GamificationErrorCode.ITEM_NOT_FOR_SALE, message);
    }
}
