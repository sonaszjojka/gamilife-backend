package pl.gamilife.gamification.service;

import jakarta.transaction.Transactional;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

import java.util.UUID;

public interface RewardService {
    void rewardUser(UUID userId, StatisticTypeEnum statisticTypeEnum);

    @Transactional
    void rewardUser(UUID userId, int experience, int money);
}
