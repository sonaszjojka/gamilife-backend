package pl.gamilife.groupshop.repository.jpa;


import pl.gamilife.groupshop.entity.GroupItemInShop;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GroupItemInShopRepositoryJpa extends JpaRepository<GroupItemInShop, UUID> {
}
