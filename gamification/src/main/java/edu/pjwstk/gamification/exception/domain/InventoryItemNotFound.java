package edu.pjwstk.gamification.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.gamification.exception.GamificationErrorCode;

public class InventoryItemNotFound extends DomainException {
    public InventoryItemNotFound(String message) {
        super(GamificationErrorCode.INVENTORY_ITEM_NOT_FOUND, message);
    }
}