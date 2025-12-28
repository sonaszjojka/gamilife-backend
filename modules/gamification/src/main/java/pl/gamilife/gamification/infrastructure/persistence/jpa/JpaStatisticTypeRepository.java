package pl.gamilife.gamification.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.domain.model.StatisticType;

public interface JpaStatisticTypeRepository extends JpaRepository<StatisticType, Integer> {
}