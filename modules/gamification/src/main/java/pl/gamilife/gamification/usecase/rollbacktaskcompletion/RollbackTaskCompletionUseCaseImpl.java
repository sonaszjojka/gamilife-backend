package pl.gamilife.gamification.usecase.rollbacktaskcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.service.UserStatisticsService;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

@Service
@AllArgsConstructor
public class RollbackTaskCompletionUseCaseImpl implements RollbackTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(RollbackTaskCompletionCommand cmd) {
        userStatisticsService.rollbackProgress(cmd.userId(), StatisticTypeEnum.COMPLETED_TASKS);
        return null;
    }
}
