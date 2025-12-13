package pl.gamilife.groupshop.application.creategroupiteminshop;

import pl.gamilife.groupshop.domain.model.GroupItemInShop;
import pl.gamilife.groupshop.domain.model.GroupShop;

import java.util.UUID;

public interface CreateGroupItemInShopMapper {

    GroupItemInShop toEntity(CreateGroupItemInShopRequest request, GroupShop groupShop, UUID groupItemId);

    CreateGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop);
}
