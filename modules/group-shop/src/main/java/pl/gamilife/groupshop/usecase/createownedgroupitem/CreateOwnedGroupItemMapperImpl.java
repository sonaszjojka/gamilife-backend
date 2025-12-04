package pl.gamilife.groupshop.usecase.createownedgroupitem;

import org.springframework.stereotype.Component;
import pl.gamilife.groupshop.entity.GroupItemInShop;
import pl.gamilife.groupshop.entity.OwnedGroupItem;

import java.time.Instant;
import java.util.UUID;

@Component
public class CreateOwnedGroupItemMapperImpl implements CreateOwnedGroupItemMapper {

    @Override
    public OwnedGroupItem toEntity(CreateOwnedGroupItemRequest request, UUID groupMemberId, GroupItemInShop groupItemInShop,
                                   UUID ownedGroupItemId, Instant useDate) {
        return OwnedGroupItem.builder()
                .ownedGroupItemId(ownedGroupItemId)
                .groupItemInShop(groupItemInShop)
                .groupMemberId(groupMemberId)
                .isUsedUp(request.isUsedUp())
                .useDate(useDate)
                .build();
    }

    @Override
    public CreateOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem) {
        return CreateOwnedGroupItemResponse.builder()
                .ownedGroupItemId(ownedGroupItem.getOwnedGroupItemId())
                .groupItemId(ownedGroupItem.getGroupItemInShop().getGroupItemId())
                .groupMemberId(ownedGroupItem.getGroupMemberId())
                .isUsedUp(ownedGroupItem.getIsUsedUp())
                .useDate(ownedGroupItem.getUseDate())
                .build();
    }
}
