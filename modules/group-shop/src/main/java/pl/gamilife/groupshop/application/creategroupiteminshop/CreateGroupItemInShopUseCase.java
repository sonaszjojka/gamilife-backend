package pl.gamilife.groupshop.application.creategroupiteminshop;

import java.util.UUID;

public interface CreateGroupItemInShopUseCase {

    CreateGroupItemInShopResponse execute(CreateGroupItemInShopRequest createGroupItemInShopRequest, UUID groupId, UUID groupShopId);
}
