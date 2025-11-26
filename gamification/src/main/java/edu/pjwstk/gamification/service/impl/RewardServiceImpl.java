package edu.pjwstk.gamification.service.impl;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.RewardedUserApiDto;
import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.model.Item;
import edu.pjwstk.gamification.model.Level;
import edu.pjwstk.gamification.model.Reward;
import edu.pjwstk.gamification.repository.LevelRepository;
import edu.pjwstk.gamification.repository.RewardRepository;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserInventoryService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
@AllArgsConstructor
@Slf4j
public class RewardServiceImpl implements RewardService {

    private final UserApi userApi;
    private final LevelRepository levelRepository;
    private final UserInventoryService userInventoryService;
    private final RewardRepository rewardRepository;

    @Override
    @Transactional
    public void rewardUser(UUID userId, StatisticTypeEnum statisticTypeEnum) {
        Optional<Reward> rewardOptional =
                rewardRepository.findByStatisticTypeId(statisticTypeEnum.getStatisticTypeId());

        if (rewardOptional.isEmpty()) {
            log.warn("Reward not found for statistic type: {}", statisticTypeEnum);
            return;
        }
        Reward reward = rewardOptional.get();

        rewardUser(userId, reward.getExperience(), reward.getMoney());
    }

    @Transactional
    @Override
    public void rewardUser(UUID userId, int experience, int money) {
        RewardedUserApiDto rewardedUser = userApi.grantRewardsToUser(userId, experience, money);

        List<Level> gainedLevels = levelRepository.findLevelsGained(
                rewardedUser.level(),
                rewardedUser.experience()
        );

        if (!gainedLevels.isEmpty()) {
            processLevelUp(rewardedUser, gainedLevels);
        }
    }

    private void processLevelUp(RewardedUserApiDto user, List<Level> gainedLevels) {
        Set<Item> rewardsForLevels = new HashSet<>();
        for (Level level : gainedLevels) {
            rewardsForLevels.addAll(level.getItems());
        }

        userInventoryService.addItemsToUsersInventory(user.userId(), rewardsForLevels);

        Level targetLevel = gainedLevels.getLast();
        userApi.levelUpUser(user.userId(), targetLevel.getLevel());
    }
}
