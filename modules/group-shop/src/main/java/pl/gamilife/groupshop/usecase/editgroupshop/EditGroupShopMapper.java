package pl.gamilife.groupshop.usecase.editgroupshop;

import pl.gamilife.groupshop.entity.GroupShop;

public interface EditGroupShopMapper {
    EditGroupShopResponse toResponse(GroupShop groupShop);
}
