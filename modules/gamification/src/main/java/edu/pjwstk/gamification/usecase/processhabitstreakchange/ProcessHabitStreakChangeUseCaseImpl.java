package edu.pjwstk.gamification.usecase.processhabitstreakchange;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@AllArgsConstructor
public class ProcessHabitStreakChangeUseCaseImpl implements ProcessHabitStreakChangeUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    public Void execute(ProcessHabitStreakChangeCommand cmd) {
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
