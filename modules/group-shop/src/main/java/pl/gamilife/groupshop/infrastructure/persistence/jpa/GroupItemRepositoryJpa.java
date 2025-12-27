package pl.gamilife.groupshop.infrastructure.persistence.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.domain.model.GroupItem;
import pl.gamilife.groupshop.domain.model.filter.GroupItemsFilter;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.UUID;

public interface GroupItemRepositoryJpa extends JpaRepository<GroupItem, UUID> {
    Page<GroupItem> findAll(GroupItemsFilter filter, Integer page, Integer pageSize);
}
