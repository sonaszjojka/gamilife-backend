package pl.gamilife.groupshop.usecase.editgroupshop;

import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.entity.GroupShop;

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
