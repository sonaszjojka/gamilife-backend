package pl.gamilife.groupshop.usecase.creategroupiteminshop;

import pl.gamilife.groupshop.entity.GroupItemInShop;
import pl.gamilife.groupshop.entity.GroupShop;

import java.util.UUID;

public interface CreateGroupItemInShopMapper {

    GroupItemInShop toEntity(CreateGroupItemInShopRequest request, GroupShop groupShop, UUID groupItemId);

    CreateGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop);
}
