package pl.gamilife.groupshop.application.createownedgroupitem;

import pl.gamilife.groupshop.domain.model.GroupItemInShop;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;

import java.time.Instant;
import java.util.UUID;

public interface CreateOwnedGroupItemMapper {

    OwnedGroupItem toEntity(CreateOwnedGroupItemRequest request, UUID groupMemberId, GroupItemInShop groupItemInShop, UUID ownedGroupItemId, Instant usedDate);

    CreateOwnedGroupItemResponse toResponse(OwnedGroupItem ownedGroupItem);
}
