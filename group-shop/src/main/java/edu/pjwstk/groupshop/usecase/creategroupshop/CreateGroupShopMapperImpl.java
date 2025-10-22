package edu.pjwstk.groupshop.usecase.creategroupshop;

import edu.pjwstk.groupshop.entity.GroupShop;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateGroupShopMapperImpl implements CreateGroupShopMapper {

    @Override
    public GroupShop toEntity(CreateGroupShopRequest request,  UUID groupId,UUID groupShopId) {
        return GroupShop.builder()
                .groupShopId(groupShopId)
                .name(request.name())
                .description(request.description())
                .groupId(groupId)
                .build();
    }
    @Override
    public CreateGroupShopResponse toResponse(GroupShop groupShop) {
        return CreateGroupShopResponse.builder()
                .groupShopId(groupShop.getGroupShopId())
                .name(groupShop.getName())
                .description(groupShop.getDescription())
                .groupId(groupShop.getGroupId())
                .build();
    }

}
