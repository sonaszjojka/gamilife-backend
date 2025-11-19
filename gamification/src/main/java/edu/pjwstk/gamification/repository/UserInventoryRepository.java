package edu.pjwstk.gamification.repository;

import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.UserInventory;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface UserInventoryRepository extends JpaRepository<UserInventory, UUID> {
    Optional<UserInventory> findByUserIdAndItem(UUID userId, Item item);

    @EntityGraph(attributePaths = {"item"})
    Optional<UserInventory> findWithItemById(UUID userInventoryId);
}
