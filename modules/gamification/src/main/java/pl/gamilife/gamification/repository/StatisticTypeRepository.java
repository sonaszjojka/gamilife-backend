package pl.gamilife.gamification.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.gamification.model.StatisticType;

public interface StatisticTypeRepository extends JpaRepository<StatisticType, Integer> {
}