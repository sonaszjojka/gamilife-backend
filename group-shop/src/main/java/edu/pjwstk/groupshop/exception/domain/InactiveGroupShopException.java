package edu.pjwstk.groupshop.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groupshop.exception.GroupShopErrorCode;

public class InactiveGroupShopException extends DomainException {
    public InactiveGroupShopException(String message) {
        super(GroupShopErrorCode.INACTIVE_GROUP_SHOP, message);
    }
}
