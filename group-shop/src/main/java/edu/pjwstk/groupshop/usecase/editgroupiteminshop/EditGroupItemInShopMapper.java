package edu.pjwstk.groupshop.usecase.editgroupiteminshop;

import edu.pjwstk.groupshop.entity.GroupItemInShop;

import java.util.UUID;

public interface EditGroupItemInShopMapper {

    public EditGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop);
}
