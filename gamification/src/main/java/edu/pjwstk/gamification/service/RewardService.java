package edu.pjwstk.gamification.service;

import edu.pjwstk.core.enums.StatisticTypeEnum;

import java.util.UUID;

public interface RewardService {
    void rewardUser(UUID userId, StatisticTypeEnum statisticTypeEnum);
}
