package pl.gamilife.gamification.repository;

import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.model.Item;
import pl.gamilife.gamification.model.UserInventoryItem;

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
