package pl.gamilife.groupshop.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class OwnedGroupItemNotFoundException extends DomainException {
    public OwnedGroupItemNotFoundException(String message) {
        super(GroupShopErrorCode.OWNED_GROUP_ITEM_NOT_FOUND, message);
    }
}
