package pl.gamilife.gamification.repository;

import pl.gamilife.gamification.model.ItemSlot;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ItemSlotRepository extends JpaRepository<ItemSlot, Integer> {
}