package edu.pjwstk.groupshop.usecase.editgroupshop;

import edu.pjwstk.groupshop.entity.GroupShop;

public interface EditGroupShopMapper {
    EditGroupShopResponse toResponse(GroupShop groupShop);
}
