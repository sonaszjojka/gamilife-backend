package edu.pjwstk.groupshop.usecase.creategroupshopforgroup;

import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import edu.pjwstk.groupshop.entity.GroupShop;

import java.util.UUID;

public interface CreateGroupShopForGroupMapper {

    GroupShop toEntity(CreateGroupShopForGroupRequestDto request, UUID groupShopId);

    CreateGroupShopForGroupResponseDto toResponse(GroupShop groupShop);
}
