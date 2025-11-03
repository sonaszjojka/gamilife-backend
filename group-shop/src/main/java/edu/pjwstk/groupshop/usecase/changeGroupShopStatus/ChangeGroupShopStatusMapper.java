package edu.pjwstk.groupshop.usecase.changeGroupShopStatus;

import edu.pjwstk.groupshop.entity.GroupShop;

public interface ChangeGroupShopStatusMapper {
    ChangeGroupShopStatusResponse toResponse(GroupShop groupShop);
}
