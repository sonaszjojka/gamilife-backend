package pl.gamilife.gamification.domain.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.model.Reward;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.model.projection.GamificationUser;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;
import pl.gamilife.gamification.domain.port.repository.RewardRepository;
import pl.gamilife.gamification.domain.service.RewardService;
import pl.gamilife.gamification.domain.service.UserInventoryService;

import java.util.*;

@Service
@AllArgsConstructor
@Slf4j
public class RewardServiceImpl implements RewardService {

    private final UserContext userContext;
    private final LevelRepository levelRepository;
    private final UserInventoryService userInventoryService;
    private final RewardRepository rewardRepository;

    @Override
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

    @Override
    public void rewardUser(UUID userId, int experience, int money) {
        GamificationUser rewardedUser = userContext.grantRewardsToUser(userId, experience, money);

        List<Level> gainedLevels = levelRepository.findLevelsGained(
                rewardedUser.level(),
                rewardedUser.experience()
        );

        if (!gainedLevels.isEmpty()) {
            processLevelUp(rewardedUser, gainedLevels);
        }
    }

    private void processLevelUp(GamificationUser user, List<Level> gainedLevels) {
        Set<Item> rewardsForLevels = new HashSet<>();
        for (Level level : gainedLevels) {
            rewardsForLevels.addAll(level.getItems());
        }

        userInventoryService.addItemsToUsersInventory(user.userId(), rewardsForLevels);

        Level targetLevel = gainedLevels.getLast();
        userContext.levelUpUser(user.userId(), targetLevel.getLevel());
    }
}
