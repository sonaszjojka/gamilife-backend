package pl.gamilife.groupshop.usecase.createownedgroupitem;

import pl.gamilife.groupshop.entity.GroupItemInShop;
import pl.gamilife.groupshop.entity.OwnedGroupItem;

import java.time.Instant;
import java.util.UUID;

public interface CreateOwnedGroupItemMapper {

    OwnedGroupItem toEntity(CreateOwnedGroupItemRequest request, UUID groupMemberId, GroupItemInShop groupItemInShop, UUID ownedGroupItemId, Instant usedDate);

    CreateOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem);
}
