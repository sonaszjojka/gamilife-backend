package edu.pjwstk.groupshop.usecase.editgroupiteminshop;

import edu.pjwstk.groupshop.entity.GroupItemInShop;

public interface EditGroupItemInShopMapper {

    EditGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop);
}
