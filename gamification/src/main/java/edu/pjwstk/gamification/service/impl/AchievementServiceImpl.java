package edu.pjwstk.gamification.service.impl;

import edu.pjwstk.gamification.model.Achievement;
import edu.pjwstk.gamification.model.UserAchievement;
import edu.pjwstk.gamification.model.UserStatistic;
import edu.pjwstk.gamification.repository.AchievementRepository;
import edu.pjwstk.gamification.repository.UserAchievementRepository;
import edu.pjwstk.gamification.service.AchievementService;
import edu.pjwstk.gamification.service.UserInventoryService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
@Slf4j
public class AchievementServiceImpl implements AchievementService {

    private final UserAchievementRepository userAchievementRepository;
    private final AchievementRepository achievementRepository;
    private final UserInventoryService userInventoryService;

    @Override
    @Transactional
    public void checkIfUserQualifiesForAchievementOfType(UserStatistic userStatistic) {
        Optional<Achievement> achievementOptional =
                achievementRepository.findWithItemsByStatisticTypeIdAndNotEarnedByUserId(
                        userStatistic.getStatisticTypeId(),
                        userStatistic.getUserId(),
                        PageRequest.of(0, 1) // LIMIT 1
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
        UserAchievement userAchievement = UserAchievement.builder()
                .achievement(achievement)
                .userId(userId)
                .build();

        userAchievementRepository.save(userAchievement);
        log.info("User earned achievement: {}", userAchievement);

        userInventoryService.addItemsToUsersInventory(userId, achievement.getItems());

        // TODO: handle money and experience
    }

}
