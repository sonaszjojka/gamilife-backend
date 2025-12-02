package edu.pjwstk.groupshop.exception;

import pl.gamilife.infrastructure.core.exception.ErrorCode;

public enum GroupShopErrorCode implements ErrorCode {
    GROUP_SHOP_ITEM_NOT_FOUND,
    GROUP_SHOP_NOT_FOUND,
    INACTIVE_GROUP_SHOP,
    INVALID_OWNED_GROUP_ITEM_DATA,
    OWNED_GROUP_ITEM_NOT_FOUND;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "GROUP_SHOP";
    }
}
