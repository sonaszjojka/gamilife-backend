package pl.gamilife.groupshop.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.groupshop.exception.GroupShopErrorCode;

public class InactiveGroupShopException extends DomainException {
    public InactiveGroupShopException(String message) {
        super(GroupShopErrorCode.INACTIVE_GROUP_SHOP, message);
    }
}
