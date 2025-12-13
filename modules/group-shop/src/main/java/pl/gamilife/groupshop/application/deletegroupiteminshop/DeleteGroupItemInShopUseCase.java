package pl.gamilife.groupshop.application.deletegroupiteminshop;

import java.util.UUID;

public interface DeleteGroupItemInShopUseCase {
    void deleteById(UUID groupItemInShopId, UUID groupId);
}
