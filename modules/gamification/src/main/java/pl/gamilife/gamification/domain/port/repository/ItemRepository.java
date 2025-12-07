package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.Item;

import java.util.Optional;
import java.util.UUID;

public interface ItemRepository {
    Optional<Item> findById(UUID itemId);
}
