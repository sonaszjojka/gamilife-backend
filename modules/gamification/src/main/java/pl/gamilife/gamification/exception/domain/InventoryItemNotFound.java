package pl.gamilife.gamification.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.gamification.exception.GamificationErrorCode;

public class InventoryItemNotFound extends DomainException {
    public InventoryItemNotFound(String message) {
        super(GamificationErrorCode.INVENTORY_ITEM_NOT_FOUND, message);
    }
}