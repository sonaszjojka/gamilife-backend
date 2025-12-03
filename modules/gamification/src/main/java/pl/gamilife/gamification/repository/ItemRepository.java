package pl.gamilife.gamification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.model.Item;

import java.util.UUID;

public interface ItemRepository extends JpaRepository<Item, UUID> {
}