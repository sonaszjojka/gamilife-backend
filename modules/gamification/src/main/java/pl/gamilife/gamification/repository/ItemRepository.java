package pl.gamilife.gamification.repository;

import pl.gamilife.gamification.model.Item;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface ItemRepository extends JpaRepository<Item, UUID> {
}