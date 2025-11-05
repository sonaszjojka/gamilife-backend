package edu.pjwstk.groupshop.repository.impl;

import edu.pjwstk.groupshop.entity.GroupItemInShop;
import edu.pjwstk.groupshop.repository.GroupItemInShopRepository;
import edu.pjwstk.groupshop.repository.jpa.GroupItemInShopRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupItemInShopRepositoryImpl implements GroupItemInShopRepository {

    private final GroupItemInShopRepositoryJpa groupItemInShopRepositoryJpa;

    public GroupItemInShopRepositoryImpl(GroupItemInShopRepositoryJpa groupItemInShopRepositoryJpa) {
        this.groupItemInShopRepositoryJpa = groupItemInShopRepositoryJpa;
    }

    @Override
    public void save(GroupItemInShop groupItemInShop) {
        groupItemInShopRepositoryJpa.save(groupItemInShop);

    }

    @Override
    public void deleteById(UUID groupItemInShopId) {
        groupItemInShopRepositoryJpa.deleteById(groupItemInShopId);
    }

    @Override
    public Optional<GroupItemInShop> findById(UUID groupItemId) {
        return groupItemInShopRepositoryJpa.findById(groupItemId);
    }
}
