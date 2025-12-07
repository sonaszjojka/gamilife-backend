package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

public interface UserInventoryItemRepository {
    UserInventoryItem save(UserInventoryItem userInventoryItem);

    void delete(UserInventoryItem userInventoryItem);

    Optional<UserInventoryItem> findWithItemById(UUID userInventoryItemId);

    List<UserInventoryItem> findAllByUserIdAndItemIn(UUID userId, Set<Item> items);

    void saveAll(List<UserInventoryItem> toSave);

    Optional<UserInventoryItem> findByUserIdAndItem(UUID userId, Item item);

    Optional<UserInventoryItem> findItemEquippedOnSlot(UUID userId, int itemSlotId, UUID newInventoryItemId);
}
