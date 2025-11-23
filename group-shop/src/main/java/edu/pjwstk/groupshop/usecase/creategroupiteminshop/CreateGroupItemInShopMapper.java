package edu.pjwstk.groupshop.usecase.creategroupiteminshop;

import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.entity.GroupShop;

import java.util.UUID;

public interface CreateGroupItemInShopMapper {

    GroupItemInShop toEntity(CreateGroupItemInShopRequest request, GroupShop groupShop, UUID groupItemId);

    CreateGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop);
}
