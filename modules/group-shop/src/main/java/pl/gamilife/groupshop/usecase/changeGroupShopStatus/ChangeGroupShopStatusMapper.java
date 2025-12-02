package pl.gamilife.groupshop.usecase.changeGroupShopStatus;

import pl.gamilife.groupshop.entity.GroupShop;

public interface ChangeGroupShopStatusMapper {
    ChangeGroupShopStatusResponse toResponse(GroupShop groupShop);
}
