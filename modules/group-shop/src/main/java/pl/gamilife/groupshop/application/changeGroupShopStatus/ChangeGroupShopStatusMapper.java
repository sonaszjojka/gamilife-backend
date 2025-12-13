package pl.gamilife.groupshop.application.changeGroupShopStatus;

import pl.gamilife.groupshop.domain.model.GroupShop;

public interface ChangeGroupShopStatusMapper {
    ChangeGroupShopStatusResponse toResponse(GroupShop groupShop);
}
