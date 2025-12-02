package pl.gamilife.groupshop.exception.domain;

import pl.gamilife.infrastructure.core.exception.DomainException;
import pl.gamilife.groupshop.exception.GroupShopErrorCode;

public class InvalidOwnedGroupItemDataException extends DomainException {
    public InvalidOwnedGroupItemDataException(String message) {
        super(GroupShopErrorCode.INVALID_OWNED_GROUP_ITEM_DATA, message);
    }
}
