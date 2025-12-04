package pl.gamilife.groupshop.repository.impl;

import org.springframework.stereotype.Repository;
import pl.gamilife.groupshop.entity.GroupItemInShop;
import pl.gamilife.groupshop.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.repository.jpa.GroupItemInShopRepositoryJpa;

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
