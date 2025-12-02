package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventoryItem;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public interface UserInventoryItemRepository extends JpaRepository<UserInventoryItem, UUID> {
    Optional<UserInventoryItem> findByUserIdAndItem(UUID userId, Item item);

    List<UserInventoryItem> findAllByUserIdAndItemIn(UUID userId, Set<Item> items);

    @EntityGraph(attributePaths = {"item"})
    Optional<UserInventoryItem> findWithItemById(UUID userInventoryId);
}
