package pl.gamilife.shared.kernel.exception.domain;

import pl.gamilife.shared.kernel.exception.CoreErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class UserHasNotEnoughMoneyException extends DomainException {
    public UserHasNotEnoughMoneyException(String message) {
        super(CoreErrorCode.USER_HAS_NOT_ENOUGH_MONEY, message);
    }
}
