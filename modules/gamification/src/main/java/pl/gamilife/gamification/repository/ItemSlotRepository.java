package pl.gamilife.gamification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.model.ItemSlot;

public interface ItemSlotRepository extends JpaRepository<ItemSlot, Integer> {
}