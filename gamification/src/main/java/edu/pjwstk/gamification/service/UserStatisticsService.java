package edu.pjwstk.gamification.service;

import edu.pjwstk.core.enums.StatisticTypeEnum;

import java.util.UUID;

public interface UserStatisticsService {
    void registerProgress(UUID userId, StatisticTypeEnum statisticTypeEnum);

    void rollbackProgress(UUID userId, StatisticTypeEnum statisticTypeEnum);
}
