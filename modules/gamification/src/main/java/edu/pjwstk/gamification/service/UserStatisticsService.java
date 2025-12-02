package edu.pjwstk.gamification.service;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

import java.util.Set;
import java.util.UUID;

public interface UserStatisticsService {
    void registerProgress(UUID userId, StatisticTypeEnum statisticTypeEnum);

    void registerProgressForAll(UUID userId, Set<StatisticTypeEnum> statisticTypes);

    void registerProgressIfHigherThan(UUID userId, StatisticTypeEnum statisticTypeEnum, Integer newValue);

    void rollbackProgress(UUID userId, StatisticTypeEnum statisticTypeEnum);
}
