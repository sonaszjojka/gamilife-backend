package pl.gamilife.group.exception.domain;

import pl.gamilife.group.exception.GroupErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class NotEnoughGroupMoneyException extends DomainException {
    public NotEnoughGroupMoneyException(String message) {
        super(GroupErrorCode.NOT_ENOUGH_GROUP_MONEY, message);
    }
}
