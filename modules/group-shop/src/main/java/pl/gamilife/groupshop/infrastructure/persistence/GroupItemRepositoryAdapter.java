package pl.gamilife.groupshop.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.filter.GroupItemsFilter;
import pl.gamilife.groupshop.domain.port.repository.GroupItemRepository;
import pl.gamilife.groupshop.infrastructure.persistence.jpa.GroupItemRepositoryJpa;
import pl.gamilife.groupshop.infrastructure.persistence.specification.GroupItemSpecificationBuilder;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class GroupItemRepositoryAdapter implements GroupItemRepository {

    private final GroupItemRepositoryJpa groupItemRepositoryJpa;
    private final GroupItemSpecificationBuilder groupItemSpecificationBuilder;

    @Override
    public void save(GroupItem groupItem) {
        groupItemRepositoryJpa.save(groupItem);

    }

    @Override
    public void deleteById(UUID groupItemInShopId) {
        groupItemRepositoryJpa.deleteById(groupItemInShopId);
    }

    @Override
    public Page<GroupItem> findAll(GroupItemsFilter filter, Integer page, Integer size) {
        return groupItemRepositoryJpa.findAll(groupItemSpecificationBuilder.build(filter), PageRequest.of(page,size));
    }

    @Override
    public Optional<GroupItem> findById(UUID groupItemId) {
        return groupItemRepositoryJpa.findById(groupItemId);
    }
}
