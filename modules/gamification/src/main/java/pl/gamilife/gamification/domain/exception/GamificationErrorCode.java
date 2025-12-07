package pl.gamilife.gamification.domain.exception;

import pl.gamilife.shared.kernel.exception.ErrorCode;

public enum GamificationErrorCode implements ErrorCode {
    ITEM_NOT_FOUND,
    ITEM_NOT_FOR_SALE,
    INVENTORY_ITEM_NOT_FOUND,
    FORBIDDEN_ITEM_ACCESS, USER_DOES_NOT_HAVE_ENOUGH_ITEMS,
    INVALID_GAMIFICATION_OPERATION;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "GAMIFICATION";
    }
}
