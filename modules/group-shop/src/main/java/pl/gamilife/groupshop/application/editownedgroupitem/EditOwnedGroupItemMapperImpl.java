package pl.gamilife.groupshop.application.editownedgroupitem;

import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;

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
