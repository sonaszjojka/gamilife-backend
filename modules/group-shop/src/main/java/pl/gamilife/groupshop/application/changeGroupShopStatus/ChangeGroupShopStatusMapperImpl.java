package pl.gamilife.groupshop.application.changeGroupShopStatus;

import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.domain.model.GroupShop;

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
