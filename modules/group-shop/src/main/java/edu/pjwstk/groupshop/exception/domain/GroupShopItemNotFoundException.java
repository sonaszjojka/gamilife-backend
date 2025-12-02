package edu.pjwstk.groupshop.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import edu.pjwstk.groupshop.exception.GroupShopErrorCode;

public class GroupShopItemNotFoundException extends DomainException {
    public GroupShopItemNotFoundException(String message) {
        super(GroupShopErrorCode.GROUP_SHOP_ITEM_NOT_FOUND, message);
    }
}
