package pl.gamilife.gamification.usecase.processpomodorotaskcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.service.RewardService;
import pl.gamilife.gamification.service.UserStatisticsService;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

@Service
@Transactional
@AllArgsConstructor
public class ProcessPomodoroTaskCompletionUseCaseImpl implements ProcessPomodoroTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    public Void execute(ProcessPomodoroTaskCompletionCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.POMODORO_TASKS_COMPLETED);

        if (!cmd.rewardGranted()) {
            rewardService.rewardUser(
                    cmd.userId(),
                    StatisticTypeEnum.POMODORO_TASKS_COMPLETED
            );
        }

        return null;
    }
}
