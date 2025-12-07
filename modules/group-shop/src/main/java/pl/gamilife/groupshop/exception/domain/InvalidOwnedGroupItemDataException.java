package pl.gamilife.groupshop.exception.domain;

import pl.gamilife.groupshop.exception.GroupShopErrorCode;
import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidOwnedGroupItemDataException extends DomainException {
    public InvalidOwnedGroupItemDataException(String message) {
        super(GroupShopErrorCode.INVALID_OWNED_GROUP_ITEM_DATA, message);
    }
}
