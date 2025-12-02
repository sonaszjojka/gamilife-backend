package edu.pjwstk.groupshop.usecase.createownedgroupitem;

import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.entity.OwnedGroupItem;

import java.time.Instant;
import java.util.UUID;

public interface CreateOwnedGroupItemMapper {

    OwnedGroupItem toEntity(CreateOwnedGroupItemRequest request, UUID groupMemberId, GroupItemInShop groupItemInShop, UUID ownedGroupItemId, Instant usedDate);

    CreateOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem);
}
