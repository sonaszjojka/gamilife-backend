package pl.gamilife.gamification.repository;

import pl.gamilife.gamification.model.Rarity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RarityRepository extends JpaRepository<Rarity, Integer> {
}