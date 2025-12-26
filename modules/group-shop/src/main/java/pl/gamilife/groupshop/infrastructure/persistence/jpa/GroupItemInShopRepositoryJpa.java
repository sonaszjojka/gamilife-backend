package pl.gamilife.groupshop.infrastructure.persistence.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.domain.model.GroupItem;

import java.util.UUID;

public interface GroupItemInShopRepositoryJpa extends JpaRepository<GroupItem, UUID> {
}
