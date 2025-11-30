package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public interface UserInventoryItemRepository extends JpaRepository<UserInventoryItem, UUID>,
        JpaSpecificationExecutor<UserInventoryItem> {

    Optional<UserInventoryItem> findByUserIdAndItem(UUID userId, Item item);

    List<UserInventoryItem> findAllByUserIdAndItemIn(UUID userId, Set<Item> items);

    @EntityGraph(attributePaths = {"item"})
    Optional<UserInventoryItem> findWithItemById(UUID userInventoryId);

    @EntityGraph(attributePaths = {"item", "item.itemSlot", "item.rarity"})
    Optional<UserInventoryItem> findWithItemDetailsById(UUID id);

    @EntityGraph(attributePaths = {"item", "item.itemSlot", "item.rarity"})
    List<UserInventoryItem> findWithItemDetailsByIdIn(List<UUID> ids);

    @Override
    @EntityGraph(attributePaths = {"item", "item.itemSlot", "item.rarity"})
    Page<UserInventoryItem> findAll(Specification<UserInventoryItem> spec, Pageable pageable);
}
