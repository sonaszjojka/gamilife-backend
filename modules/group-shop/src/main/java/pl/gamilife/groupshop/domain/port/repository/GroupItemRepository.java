package pl.gamilife.groupshop.domain.port.repository;

import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.filter.GroupItemsFilter;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Optional;
import java.util.UUID;

public interface GroupItemRepository {
    void save(GroupItem groupItem);

    void deleteById(UUID groupItemInShopId);


    Page<GroupItem> findAll(GroupItemsFilter filter, Integer page, Integer pageSize);

    Optional<GroupItem> findById(UUID groupItemId);
}
