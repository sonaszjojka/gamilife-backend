package pl.gamilife.groupshop.infrastructure.persistence.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.domain.model.GroupItemInShop;

import java.util.UUID;

public interface GroupItemInShopRepositoryJpa extends JpaRepository<GroupItemInShop, UUID> {
}
