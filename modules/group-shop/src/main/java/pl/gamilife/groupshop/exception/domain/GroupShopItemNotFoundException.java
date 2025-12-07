package pl.gamilife.groupshop.exception.domain;

import pl.gamilife.groupshop.exception.GroupShopErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupShopItemNotFoundException extends DomainException {
    public GroupShopItemNotFoundException(String message) {
        super(GroupShopErrorCode.GROUP_SHOP_ITEM_NOT_FOUND, message);
    }
}
