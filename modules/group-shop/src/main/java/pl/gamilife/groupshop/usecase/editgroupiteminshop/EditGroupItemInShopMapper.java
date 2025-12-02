package pl.gamilife.groupshop.usecase.editgroupiteminshop;

import pl.gamilife.groupshop.entity.GroupItemInShop;

public interface EditGroupItemInShopMapper {

    EditGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop);
}
