package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.domain.model.Rarity;

public interface JpaRarityRepository extends JpaRepository<Rarity, Integer> {
}