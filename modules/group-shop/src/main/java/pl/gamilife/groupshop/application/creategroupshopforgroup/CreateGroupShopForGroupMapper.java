package pl.gamilife.groupshop.application.creategroupshopforgroup;

import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import pl.gamilife.groupshop.domain.model.GroupShop;

import java.util.UUID;

public interface CreateGroupShopForGroupMapper {

    GroupShop toEntity(CreateGroupShopForGroupRequestDto request, UUID groupShopId);

    CreateGroupShopForGroupResponseDto toResponse(GroupShop groupShop);
}
