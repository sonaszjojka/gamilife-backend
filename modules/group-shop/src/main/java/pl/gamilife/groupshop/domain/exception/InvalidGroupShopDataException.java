package pl.gamilife.groupshop.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidGroupShopDataException extends DomainException {
    public InvalidGroupShopDataException(String message) {
        super(GroupShopErrorCode.INVALID_GROUP_SHOP_ITEM_DATA,message);
    }
}
