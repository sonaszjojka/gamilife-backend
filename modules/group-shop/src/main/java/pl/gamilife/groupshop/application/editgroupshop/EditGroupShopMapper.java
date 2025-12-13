package pl.gamilife.groupshop.application.editgroupshop;

import pl.gamilife.groupshop.domain.model.GroupShop;

public interface EditGroupShopMapper {
    EditGroupShopResponse toResponse(GroupShop groupShop);
}
