package edu.pjwstk.gamification.usecase.rollbackpomodorotaskcompletion;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RollbackPomodoroTaskCompletionUseCaseImpl implements RollbackPomodoroTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void executeInternal(RollbackPomodoroTaskCompletionCommand cmd) {
        userStatisticsService.rollbackProgress(cmd.userId(), StatisticTypeEnum.POMODORO_TASKS_COMPLETED);
        return null;
    }
}
