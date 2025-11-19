package edu.pjwstk.gamification.exception;

import edu.pjwstk.core.exception.ErrorCode;

public enum GamificationErrorCode implements ErrorCode {
    ITEM_NOT_FOUND,
    ITEM_NOT_FOR_SALE;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "GAMIFICATION";
    }
}
