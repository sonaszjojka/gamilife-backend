package edu.pjwstk.gamification.exception;

import edu.pjwstk.core.exception.ErrorCode;

public enum GamificationErrorCode implements ErrorCode {
    ITEM_NOT_FOUND,
    ITEM_NOT_FOR_SALE,
    INVENTORY_ITEM_NOT_FOUND,
    FORBIDDEN_ITEM_ACCESS, USER_DOES_NOT_HAVE_ENOUGH_ITEMS;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "GAMIFICATION";
    }
}
