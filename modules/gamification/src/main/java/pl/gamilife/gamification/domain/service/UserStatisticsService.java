package pl.gamilife.gamification.domain.service;

import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;

import java.util.Set;
import java.util.UUID;

public interface UserStatisticsService {
    void registerProgress(UUID userId, StatisticTypeEnum statisticTypeEnum);

    void registerProgressForAll(UUID userId, Set<StatisticTypeEnum> statisticTypes);

    void registerProgressIfHigherThan(UUID userId, StatisticTypeEnum statisticTypeEnum, Integer newValue);

    void rollbackProgress(UUID userId, StatisticTypeEnum statisticTypeEnum);
}
