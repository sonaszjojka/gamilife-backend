package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.filter.StoreItemsFilter;
import pl.gamilife.shared.kernel.architecture.Page;

import java.util.Optional;
import java.util.UUID;

public interface ItemRepository {
    Optional<Item> findWithSlotAndRarityById(UUID itemId);

    Page<Item> findAll(StoreItemsFilter filter, Integer page, Integer size);

    Optional<Item> findById(UUID id);
}
