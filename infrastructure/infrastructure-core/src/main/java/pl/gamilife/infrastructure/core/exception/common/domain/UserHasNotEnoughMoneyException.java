package pl.gamilife.infrastructure.core.exception.common.domain;

import pl.gamilife.infrastructure.core.exception.CommonErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserHasNotEnoughMoneyException extends DomainException {
    public UserHasNotEnoughMoneyException(String message) {
        super(CommonErrorCode.USER_HAS_NOT_ENOUGH_MONEY, message);
    }
}
