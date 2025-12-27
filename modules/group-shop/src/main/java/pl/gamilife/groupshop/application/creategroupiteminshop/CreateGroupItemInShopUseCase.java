package pl.gamilife.groupshop.application.creategroupiteminshop;

import java.util.UUID;

public interface CreateGroupItemInShopUseCase {

    CreateGroupItemInShopResult execute(CreateGroupItemInShopCommand createGroupItemInShopCommand);
}
