package pl.gamilife.gamification.usecase.processhabitstreakchange;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import pl.gamilife.gamification.service.RewardService;
import pl.gamilife.gamification.service.UserStatisticsService;
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
