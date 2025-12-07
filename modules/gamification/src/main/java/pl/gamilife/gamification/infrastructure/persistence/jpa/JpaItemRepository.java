package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.domain.model.Item;

import java.util.UUID;

public interface JpaItemRepository extends JpaRepository<Item, UUID> {
}