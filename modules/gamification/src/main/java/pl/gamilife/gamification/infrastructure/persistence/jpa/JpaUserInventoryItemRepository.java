package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public interface JpaUserInventoryItemRepository extends JpaRepository<UserInventoryItem, UUID>, JpaSpecificationExecutor<UserInventoryItem> {
    Optional<UserInventoryItem> findByUserIdAndItem(UUID userId, Item item);

    List<UserInventoryItem> findAllByUserIdAndItemIn(UUID userId, Set<Item> items);

    @EntityGraph(attributePaths = {"item"})
    Optional<UserInventoryItem> findWithItemById(UUID userInventoryId);

    @Query(
            """
                        SELECT uii
                        FROM UserInventoryItem uii
                        LEFT JOIN FETCH uii.item i
                        WHERE uii.userId = :userId
                          AND i.itemSlotId = :itemSlotId
                          AND uii.isEquipped = true
                          AND uii.id != :newInventoryItemId
                    """
    )
    Optional<UserInventoryItem> findItemEquippedOnSlot(
            @Param("userId") UUID userId,
            @Param("itemSlotId") int itemSlotId,
            @Param("newInventoryItemId") UUID newInventoryItemId);

    @EntityGraph(attributePaths = {"item", "item.itemSlot", "item.rarity"})
    Optional<UserInventoryItem> findWithItemDetailsById(UUID id);

    @EntityGraph(attributePaths = {"item", "item.itemSlot", "item.rarity"})
    List<UserInventoryItem> findWithItemDetailsByIdIn(List<UUID> ids);

    @Override
    @EntityGraph(attributePaths = {"item", "item.itemSlot", "item.rarity"})
    Page<UserInventoryItem> findAll(Specification<UserInventoryItem> spec, Pageable pageable);
}
