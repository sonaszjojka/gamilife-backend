package edu.pjwstk.groupshop.usecase.editgroupshop;

import edu.pjwstk.groupshop.entity.GroupShop;
import org.springframework.stereotype.Component;

@Component
public class EditGroupShopMapperImpl implements EditGroupShopMapper {
    @Override
    public EditGroupShopResponse toResponse(GroupShop groupShop) {
        return EditGroupShopResponse.builder()
                .groupShopId(groupShop.getGroupShopId())
                .name(groupShop.getName())
                .description(groupShop.getDescription())
                .groupId(groupShop.getGroupId())
                .build();
    }
}
