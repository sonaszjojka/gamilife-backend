package pl.gamilife.gamification.repository;

import pl.gamilife.gamification.model.StatisticType;
import org.springframework.data.jpa.repository.JpaRepository;

public interface StatisticTypeRepository extends JpaRepository<StatisticType, Integer> {
}