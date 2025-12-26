package pl.gamilife.groupshop.domain.port.repository;

import pl.gamilife.groupshop.domain.model.GroupItem;

import java.util.Optional;
import java.util.UUID;

public interface GroupItemInShopRepository {
    void save(GroupItem groupItem);

    void deleteById(UUID groupItemInShopId);

    Optional<GroupItem> findById(UUID groupItemId);
}
