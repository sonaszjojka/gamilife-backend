package pl.gamilife.gamification.usecase.rollbackpomodorotaskcompletion;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import pl.gamilife.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RollbackPomodoroTaskCompletionUseCaseImpl implements RollbackPomodoroTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(RollbackPomodoroTaskCompletionCommand cmd) {
        userStatisticsService.rollbackProgress(cmd.userId(), StatisticTypeEnum.POMODORO_TASKS_COMPLETED);
        return null;
    }
}
