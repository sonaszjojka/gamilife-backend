package pl.gamilife.groupshop.usecase.creategroupshopforgroup;

import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import pl.gamilife.groupshop.entity.GroupShop;

import java.util.UUID;

public interface CreateGroupShopForGroupMapper {

    GroupShop toEntity(CreateGroupShopForGroupRequestDto request, UUID groupShopId);

    CreateGroupShopForGroupResponseDto toResponse(GroupShop groupShop);
}
