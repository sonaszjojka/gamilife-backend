package edu.pjwstk.groupshop.repository;

import edu.pjwstk.groupshop.entity.GroupItemInShop;

import java.util.Optional;
import java.util.UUID;

public interface GroupItemInShopRepository {
    void save(GroupItemInShop groupItemInShop);

    void deleteById(UUID groupItemInShopId);

    Optional<GroupItemInShop> findById(UUID groupItemId);
}
