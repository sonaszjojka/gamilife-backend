package pl.gamilife.groupshop.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class GroupShopNotFoundException extends DomainException {
    public GroupShopNotFoundException(String message) {
        super(GroupShopErrorCode.GROUP_SHOP_NOT_FOUND, message);
    }
}
