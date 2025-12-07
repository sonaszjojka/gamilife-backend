package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.port.repository.UserInventoryItemRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaUserInventoryItemRepository;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class UserInventoryItemRepositoryAdapter implements UserInventoryItemRepository {

    private final JpaUserInventoryItemRepository jpaUserInventoryItemRepository;

    @Override
    public UserInventoryItem save(UserInventoryItem userInventoryItem) {
        return jpaUserInventoryItemRepository.save(userInventoryItem);
    }

    @Override
    public void delete(UserInventoryItem userInventoryItem) {
        jpaUserInventoryItemRepository.delete(userInventoryItem);
    }

    @Override
    public Optional<UserInventoryItem> findWithItemById(UUID userInventoryItemId) {
        return jpaUserInventoryItemRepository.findWithItemById(userInventoryItemId);
    }

    @Override
    public List<UserInventoryItem> findAllByUserIdAndItemIn(UUID userId, Set<Item> items) {
        return jpaUserInventoryItemRepository.findAllByUserIdAndItemIn(userId, items);
    }

    @Override
    public void saveAll(List<UserInventoryItem> toSave) {
        jpaUserInventoryItemRepository.saveAll(toSave);
    }

    @Override
    public Optional<UserInventoryItem> findByUserIdAndItem(UUID userId, Item item) {
        return jpaUserInventoryItemRepository.findByUserIdAndItem(userId, item);
    }

    @Override
    public Optional<UserInventoryItem> findItemEquippedOnSlot(UUID userId, int itemSlotId, UUID newInventoryItemId) {
        return jpaUserInventoryItemRepository.findItemEquippedOnSlot(userId, itemSlotId, newInventoryItemId);
    }
}
