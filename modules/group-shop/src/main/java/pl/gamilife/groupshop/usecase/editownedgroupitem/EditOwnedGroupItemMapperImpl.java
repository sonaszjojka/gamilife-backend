package pl.gamilife.groupshop.usecase.editownedgroupitem;

import pl.gamilife.groupshop.entity.OwnedGroupItem;
import org.springframework.stereotype.Component;

@Component
public class EditOwnedGroupItemMapperImpl implements EditOwnedGroupItemMapper {


    @Override
    public EditOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem) {
        return EditOwnedGroupItemResponse.builder()
                .ownedGroupItemId(ownedGroupItem.getOwnedGroupItemId())
                .groupItemId(ownedGroupItem.getGroupItemInShop().getGroupItemId())
                .groupMemberId(ownedGroupItem.getGroupMemberId())
                .isUsedUp(ownedGroupItem.getIsUsedUp())
                .useDate(ownedGroupItem.getUseDate())
                .build();
    }
}
