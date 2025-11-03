package edu.pjwstk.groupshop.usecase.creategroupshopforgroup;

import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.common.groupshopApi.dto.CreateGroupShopForGroupResponseDto;
import edu.pjwstk.groupshop.entity.GroupShop;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopRequest;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopResponse;

import java.util.UUID;

public interface CreateGroupShopForGroupMapper {

    GroupShop toEntity(CreateGroupShopForGroupRequestDto request, UUID groupShopId);

    CreateGroupShopForGroupResponseDto toResponse(GroupShop groupShop);
}
