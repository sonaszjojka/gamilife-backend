package pl.gamilife.gamification.domain.service.impl;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Achievement;
import pl.gamilife.gamification.domain.model.UserAchievement;
import pl.gamilife.gamification.domain.model.UserStatistic;
import pl.gamilife.gamification.domain.port.repository.AchievementRepository;
import pl.gamilife.gamification.domain.port.repository.UserAchievementRepository;
import pl.gamilife.gamification.domain.service.AchievementService;
import pl.gamilife.gamification.domain.service.RewardService;
import pl.gamilife.gamification.domain.service.UserInventoryService;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
@Slf4j
public class AchievementServiceImpl implements AchievementService {

    private final UserAchievementRepository userAchievementRepository;
    private final AchievementRepository achievementRepository;
    private final UserInventoryService userInventoryService;
    private final RewardService rewardService;

    @Override
    public void checkIfUserQualifiesForAchievementOfType(UserStatistic userStatistic) {
        Optional<Achievement> achievementOptional =
                achievementRepository.findWithItemsByStatisticTypeIdAndNotEarnedByUserId(
                        userStatistic.getStatisticTypeId(),
                        userStatistic.getUserId()
                );

        // User has all achievements
        if (achievementOptional.isEmpty()) {
            log.info("User has all achievements of type: {}", userStatistic.getStatisticTypeEnum());
            return;
        }

        Achievement achievement = achievementOptional.get();
        if (userStatistic.getCount() >= achievement.getGoal()) {
            assignAchievementAndRewardsToUser(achievement, userStatistic.getUserId());
        }
    }

    private void assignAchievementAndRewardsToUser(Achievement achievement, UUID userId) {
        UserAchievement userAchievement = UserAchievement.create(userId, achievement);

        userAchievementRepository.save(userAchievement);
        log.info("User earned achievement: {}", userAchievement);

        userInventoryService.addItemsToUsersInventory(userId, achievement.getItems());

        rewardService.rewardUser(userId, achievement.getExperienceReward(), achievement.getMoneyReward());
    }

}
