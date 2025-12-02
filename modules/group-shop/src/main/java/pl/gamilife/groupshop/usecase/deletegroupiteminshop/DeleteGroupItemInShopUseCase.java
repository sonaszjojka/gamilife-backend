package pl.gamilife.groupshop.usecase.deletegroupiteminshop;

import java.util.UUID;

public interface DeleteGroupItemInShopUseCase {
    void deleteById(UUID groupItemInShopId, UUID groupId);
}
