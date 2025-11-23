package edu.pjwstk.groupshop.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groupshop.exception.GroupShopErrorCode;

public class InvalidOwnedGroupItemDataException extends DomainException {
    public InvalidOwnedGroupItemDataException(String message) {
        super(GroupShopErrorCode.INVALID_OWNED_GROUP_ITEM_DATA, message);
    }
}
