package edu.pjwstk.core.exception.common.domain;

import edu.pjwstk.core.exception.CommonErrorCode;
import edu.pjwstk.core.exception.DomainException;

public class UserHasNotEnoughMoneyException extends DomainException {
    public UserHasNotEnoughMoneyException(String message) {
        super(CommonErrorCode.USER_HAS_NOT_ENOUGH_MONEY, message);
    }
}
