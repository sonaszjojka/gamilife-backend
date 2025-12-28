package pl.gamilife.groupshop.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class InactiveGroupShopException extends DomainException {
    public InactiveGroupShopException(String message) {
        super(GroupShopErrorCode.INACTIVE_GROUP_SHOP, message);
    }
}
