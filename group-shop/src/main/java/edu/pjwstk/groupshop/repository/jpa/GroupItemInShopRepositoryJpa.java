package edu.pjwstk.groupshop.repository.jpa;


import edu.pjwstk.groupshop.entity.GroupItemInShop;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface GroupItemInShopRepositoryJpa extends JpaRepository<GroupItemInShop, UUID> {
}
