package pl.gamilife.gamification.domain.port.repository;

import pl.gamilife.gamification.domain.model.StatisticType;

import java.util.List;

public interface StatisticTypeRepository {
    List<StatisticType> findAll();
}
