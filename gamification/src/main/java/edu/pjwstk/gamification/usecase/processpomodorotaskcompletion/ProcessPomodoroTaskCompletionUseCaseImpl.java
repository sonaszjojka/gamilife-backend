package edu.pjwstk.gamification.usecase.processpomodorotaskcompletion;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class ProcessPomodoroTaskCompletionUseCaseImpl implements ProcessPomodoroTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    @Transactional
    public Void executeInternal(ProcessPomodoroTaskCompletionCommand cmd) {
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
