package edu.pjwstk.groupshop.usecase.editgroupiteminshop;

import java.util.UUID;

public interface EditGroupItemInShopUseCase {

    EditGroupItemInShopResponse execute(UUID groupItemId, UUID groupId, EditGroupItemInShopRequest editGroupItemInShopRequest);
}
