package pl.gamilife.groupshop.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidOwnedGroupItemDataException extends DomainException {
    public InvalidOwnedGroupItemDataException(String message) {
        super(GroupShopErrorCode.INVALID_OWNED_GROUP_ITEM_DATA, message);
    }
}
