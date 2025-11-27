package edu.pjwstk.gamification.service;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import jakarta.transaction.Transactional;

import java.util.UUID;

public interface RewardService {
    void rewardUser(UUID userId, StatisticTypeEnum statisticTypeEnum);

    @Transactional
    void rewardUser(UUID userId, int experience, int money);
}
