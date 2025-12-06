package pl.gamilife.gamification.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Repository;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.port.repository.ItemRepository;
import pl.gamilife.gamification.infrastructure.persistence.jpa.JpaItemRepository;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class ItemRepositoryAdapter implements ItemRepository {

    private final JpaItemRepository jpaItemRepository;

    @Override
    public Optional<Item> findById(UUID itemId) {
        return jpaItemRepository.findById(itemId);
    }
}
