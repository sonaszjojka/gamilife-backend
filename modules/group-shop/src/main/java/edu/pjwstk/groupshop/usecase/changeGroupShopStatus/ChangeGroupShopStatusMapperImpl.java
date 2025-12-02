package edu.pjwstk.groupshop.usecase.changeGroupShopStatus;

import edu.pjwstk.groupshop.entity.GroupShop;
import org.springframework.stereotype.Component;

@Component
public class ChangeGroupShopStatusMapperImpl implements ChangeGroupShopStatusMapper {
    @Override
    public ChangeGroupShopStatusResponse toResponse(GroupShop groupShop) {
        return ChangeGroupShopStatusResponse.builder()
                .groupShopId(groupShop.getGroupShopId())
                .name(groupShop.getName())
                .description(groupShop.getDescription())
                .groupId(groupShop.getGroupId())
                .isActive(groupShop.getIsActive())
                .build();
    }
}
