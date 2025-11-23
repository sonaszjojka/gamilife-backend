package edu.pjwstk.gamification.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.gamification.exception.GamificationErrorCode;

public class ItemNotFoundException extends DomainException {
    public ItemNotFoundException(String message) {
        super(GamificationErrorCode.ITEM_NOT_FOUND, message);
    }
}
