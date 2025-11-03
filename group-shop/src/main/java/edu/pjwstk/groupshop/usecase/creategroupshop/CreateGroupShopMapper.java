package edu.pjwstk.groupshop.usecase.creategroupshop;

import edu.pjwstk.groupshop.entity.GroupShop;

import java.util.UUID;

public interface CreateGroupShopMapper {

    GroupShop toEntity(CreateGroupShopRequest request, UUID groupId,UUID groupShopId);

    CreateGroupShopResponse toResponse(GroupShop groupShop);
}
