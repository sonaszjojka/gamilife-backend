package edu.pjwstk.gamification.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.gamification.exception.GamificationErrorCode;

public class ForbiddenItemAccessException extends DomainException {
    public ForbiddenItemAccessException(String message) {
        super(GamificationErrorCode.FORBIDDEN_ITEM_ACCESS, message);
    }
}
