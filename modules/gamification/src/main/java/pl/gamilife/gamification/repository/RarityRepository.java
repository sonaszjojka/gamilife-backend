package pl.gamilife.gamification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.model.Rarity;

public interface RarityRepository extends JpaRepository<Rarity, Integer> {
}