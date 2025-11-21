package edu.pjwstk.gamification.service;

import edu.pjwstk.gamification.model.Achievement;
import edu.pjwstk.gamification.model.UserAchievement;
import edu.pjwstk.gamification.model.UserStatistic;
import edu.pjwstk.gamification.repository.AchievementRepository;
import edu.pjwstk.gamification.repository.UserAchievementRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@AllArgsConstructor
@Slf4j
public class AchievementServiceImpl implements AchievementService {

    private final UserAchievementRepository userAchievementRepository;
    private final AchievementRepository achievementRepository;

    @Override
    @Transactional
    public void checkIfUserQualifiesForAchievementOfType(UserStatistic userStatistic) {
        Optional<Achievement> achievementOptional = achievementRepository.findByStatisticTypeIdAndNotEarnedByUserId(
                userStatistic.getStatisticTypeId(),
                userStatistic.getUserId(),
                PageRequest.of(0, 1)
        );

        // User has all achievements
        if (achievementOptional.isEmpty()) {
            log.info("User has all achievements of type: {}", userStatistic.getStatisticTypeEnum());
            return;
        }

        // Assign achievement to user
        UserAchievement userAchievement = UserAchievement.builder()
                .achievement(achievementOptional.get())
                .userId(userStatistic.getUserId())
                .build();
        userAchievementRepository.save(userAchievement);
        log.info("User earned achievement: {}", userAchievement);
    }
}
