package pl.gamilife.groupshop.exception.domain;

import pl.gamilife.groupshop.exception.GroupShopErrorCode;
import pl.gamilife.infrastructure.core.exception.DomainException;

public class InactiveGroupShopException extends DomainException {
    public InactiveGroupShopException(String message) {
        super(GroupShopErrorCode.INACTIVE_GROUP_SHOP, message);
    }
}
