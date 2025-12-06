package pl.gamilife.gamification.domain.service;

import jakarta.transaction.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;

import java.util.UUID;

public interface RewardService {
    void rewardUser(UUID userId, StatisticTypeEnum statisticTypeEnum);

    @Transactional
    void rewardUser(UUID userId, int experience, int money);
}
