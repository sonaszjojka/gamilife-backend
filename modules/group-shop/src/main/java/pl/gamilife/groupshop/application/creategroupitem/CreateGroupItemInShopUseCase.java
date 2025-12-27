package pl.gamilife.groupshop.application.creategroupitem;

import pl.gamilife.shared.kernel.architecture.UseCase;

public interface CreateGroupItemInShopUseCase extends UseCase<CreateGroupItemInShopCommand, CreateGroupItemInShopResult> {

    CreateGroupItemInShopResult execute(CreateGroupItemInShopCommand createGroupItemInShopCommand);
}
