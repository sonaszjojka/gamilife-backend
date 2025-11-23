package edu.pjwstk.gamification.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.gamification.exception.GamificationErrorCode;

public class ItemIsNotForSaleException extends DomainException {
    public ItemIsNotForSaleException(String message) {
        super(GamificationErrorCode.ITEM_NOT_FOR_SALE, message);
    }
}
