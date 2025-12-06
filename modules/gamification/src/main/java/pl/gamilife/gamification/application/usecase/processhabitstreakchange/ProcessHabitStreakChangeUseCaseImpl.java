package pl.gamilife.gamification.application.usecase.processhabitstreakchange;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.service.RewardService;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

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
