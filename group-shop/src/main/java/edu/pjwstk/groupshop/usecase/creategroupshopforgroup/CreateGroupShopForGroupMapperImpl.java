package edu.pjwstk.groupshop.usecase.creategroupshopforgroup;

import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupResponseDto;
import edu.pjwstk.groupshop.entity.GroupShop;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateGroupShopForGroupMapperImpl implements CreateGroupShopForGroupMapper {

    @Override
    public GroupShop toEntity(CreateGroupShopForGroupRequestDto request, UUID groupShopId) {
        return GroupShop.builder()
                .groupShopId(groupShopId)
                .name(request.name())
                .description(request.description())
                .groupId(request.groupId())
                .isActive(true)
                .build();
    }

    @Override
    public CreateGroupShopForGroupResponseDto toResponse(GroupShop groupShop) {
        return CreateGroupShopForGroupResponseDto.builder()
                .groupShopId(groupShop.getGroupShopId())
                .name(groupShop.getName())
                .description(groupShop.getDescription())
                .build();
    }

}
