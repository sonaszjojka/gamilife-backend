package pl.gamilife.gamification.domain.exception;

import pl.gamilife.infrastructure.core.exception.DomainException;

public class InventoryItemNotFound extends DomainException {
    public InventoryItemNotFound(String message) {
        super(GamificationErrorCode.INVENTORY_ITEM_NOT_FOUND, message);
    }
}