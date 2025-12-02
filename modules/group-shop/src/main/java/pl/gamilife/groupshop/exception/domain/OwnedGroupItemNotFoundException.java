package pl.gamilife.groupshop.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.groupshop.exception.GroupShopErrorCode;

public class OwnedGroupItemNotFoundException extends DomainException {
    public OwnedGroupItemNotFoundException(String message) {
        super(GroupShopErrorCode.OWNED_GROUP_ITEM_NOT_FOUND, message);
    }
}
