package edu.pjwstk.gamification.usecase.processhabitstreakchange;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class ProcessHabitStreakChangeUseCaseImpl implements ProcessHabitStreakChangeUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    @Transactional
    public Void executeInternal(ProcessHabitStreakChangeCommand cmd) {
        userStatisticsService.registerProgressIfHigherThan(
                cmd.userId(),
                StatisticTypeEnum.HABIT_STREAK,
                cmd.streakValue()
        );

        rewardService.rewardUser(
                cmd.userId(),
                StatisticTypeEnum.HABIT_STREAK
        );

        return null;
    }
}
