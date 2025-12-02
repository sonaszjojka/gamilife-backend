package edu.pjwstk.gamification.usecase.processpomodorotaskcompletion;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
