package pl.gamilife.groupshop.domain.port.repository;

import pl.gamilife.groupshop.domain.model.GroupItemInShop;

import java.util.Optional;
import java.util.UUID;

public interface GroupItemInShopRepository {
    void save(GroupItemInShop groupItemInShop);

    void deleteById(UUID groupItemInShopId);

    Optional<GroupItemInShop> findById(UUID groupItemId);
}
