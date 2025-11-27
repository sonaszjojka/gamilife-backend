package edu.pjwstk.groupshop.exception.domain;

import edu.pjwstk.core.exception.DomainException;
import edu.pjwstk.groupshop.exception.GroupShopErrorCode;

public class OwnedGroupItemNotFoundException extends DomainException {
    public OwnedGroupItemNotFoundException(String message) {
        super(GroupShopErrorCode.OWNED_GROUP_ITEM_NOT_FOUND, message);
    }
}
