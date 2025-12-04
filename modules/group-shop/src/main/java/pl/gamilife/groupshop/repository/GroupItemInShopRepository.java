package pl.gamilife.groupshop.repository;

import pl.gamilife.groupshop.entity.GroupItemInShop;

import java.util.Optional;
import java.util.UUID;

public interface GroupItemInShopRepository {
    void save(GroupItemInShop groupItemInShop);

    void deleteById(UUID groupItemInShopId);

    Optional<GroupItemInShop> findById(UUID groupItemId);
}
