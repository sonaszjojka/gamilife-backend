package pl.gamilife.groupshop.repository.jpa;


import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.entity.GroupItemInShop;

import java.util.UUID;

public interface GroupItemInShopRepositoryJpa extends JpaRepository<GroupItemInShop, UUID> {
}
