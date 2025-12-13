package pl.gamilife.groupshop.application.editgroupiteminshop;

import pl.gamilife.groupshop.domain.model.GroupItemInShop;

public interface EditGroupItemInShopMapper {

    EditGroupItemInShopResponse toResponse(GroupItemInShop groupItemInShop);
}
