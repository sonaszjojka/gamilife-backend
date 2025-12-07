package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.UserInventoryItem;
import pl.gamilife.gamification.domain.model.filter.UserInventoryItemFilter;
import pl.gamilife.gamification.domain.port.repository.UserInventoryItemRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaUserInventoryItemRepository;
import pl.gamilife.gamification.infrastructure.persistence.specification.UserInventoryItemSpecificationBuilder;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class UserInventoryItemRepositoryAdapter implements UserInventoryItemRepository {

    private final JpaUserInventoryItemRepository jpaUserInventoryItemRepository;
    private final UserInventoryItemSpecificationBuilder specificationBuilder;

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

    @Override
    public List<UserInventoryItem> findWithItemDetailsByIdIn(List<UUID> itemIds) {
        return jpaUserInventoryItemRepository.findWithItemDetailsByIdIn(itemIds);
    }

    @Override
    public Page<UserInventoryItem> findAll(UserInventoryItemFilter userInventoryItemFilter, Integer page, Integer size) {
        org.springframework.data.domain.Page<UserInventoryItem> result = jpaUserInventoryItemRepository.findAll(
                specificationBuilder.build(userInventoryItemFilter),
                PageRequest.of(page, size)
        );

        return new Page<>(
                result.getContent(),
                result.getTotalElements(),
                result.getTotalPages(),
                result.getNumber(),
                result.getSize()
        );
    }
}
