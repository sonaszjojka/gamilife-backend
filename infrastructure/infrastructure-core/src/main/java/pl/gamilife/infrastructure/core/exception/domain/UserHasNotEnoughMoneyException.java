package pl.gamilife.infrastructure.core.exception.domain;

import pl.gamilife.infrastructure.core.exception.CoreErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class UserHasNotEnoughMoneyException extends DomainException {
    public UserHasNotEnoughMoneyException(String message) {
        super(CoreErrorCode.USER_HAS_NOT_ENOUGH_MONEY, message);
    }
}
