package edu.pjwstk.groupshop.usecase.editgroupshop;

import edu.pjwstk.groupshop.entity.GroupShop;

import java.util.UUID;

public interface EditGroupShopMapper {
    EditGroupShopResponse toResponse(GroupShop groupShop);
}
