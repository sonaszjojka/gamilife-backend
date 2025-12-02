package edu.pjwstk.groupshop.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groupshop.exception.GroupShopErrorCode;

public class GroupShopNotFoundException extends DomainException {
    public GroupShopNotFoundException(String message) {
        super(GroupShopErrorCode.GROUP_SHOP_NOT_FOUND, message);
    }
}
