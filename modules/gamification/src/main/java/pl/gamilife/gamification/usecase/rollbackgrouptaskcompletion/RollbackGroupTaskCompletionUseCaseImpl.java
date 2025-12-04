package pl.gamilife.gamification.usecase.rollbackgrouptaskcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.service.UserStatisticsService;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

@Service
@AllArgsConstructor
public class RollbackGroupTaskCompletionUseCaseImpl implements RollbackGroupTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(RollbackGroupTaskCompletionCommand cmd) {
        userStatisticsService.rollbackProgress(cmd.userId(), StatisticTypeEnum.GROUP_TASKS_COMPLETED);
        return null;
    }
}
