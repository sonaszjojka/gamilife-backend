package pl.gamilife.groupshop.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.port.repository.GroupItemInShopRepository;
import pl.gamilife.groupshop.infrastructure.persistence.jpa.GroupItemInShopRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupItemInShopRepositoryAdapter implements GroupItemInShopRepository {

    private final GroupItemInShopRepositoryJpa groupItemInShopRepositoryJpa;

    public GroupItemInShopRepositoryAdapter(GroupItemInShopRepositoryJpa groupItemInShopRepositoryJpa) {
        this.groupItemInShopRepositoryJpa = groupItemInShopRepositoryJpa;
    }

    @Override
    public void save(GroupItem groupItem) {
        groupItemInShopRepositoryJpa.save(groupItem);

    }

    @Override
    public void deleteById(UUID groupItemInShopId) {
        groupItemInShopRepositoryJpa.deleteById(groupItemInShopId);
    }

    @Override
    public Optional<GroupItem> findById(UUID groupItemId) {
        return groupItemInShopRepositoryJpa.findById(groupItemId);
    }
}
