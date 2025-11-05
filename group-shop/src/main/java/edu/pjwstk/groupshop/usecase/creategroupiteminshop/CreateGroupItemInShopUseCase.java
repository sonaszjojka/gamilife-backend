package edu.pjwstk.groupshop.usecase.creategroupiteminshop;

import edu.pjwstk.groupshop.entity.GroupItemInShop;

import java.util.UUID;

public interface CreateGroupItemInShopUseCase {

    CreateGroupItemInShopResponse execute(CreateGroupItemInShopRequest createGroupItemInShopRequest, UUID groupId, UUID groupShopId);
}
