package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.domain.model.ItemSlot;

public interface JpaItemSlotRepository extends JpaRepository<ItemSlot, Integer> {
}