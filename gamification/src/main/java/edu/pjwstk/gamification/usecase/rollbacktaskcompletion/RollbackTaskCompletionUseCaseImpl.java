package edu.pjwstk.gamification.usecase.rollbacktaskcompletion;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class RollbackTaskCompletionUseCaseImpl implements RollbackTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void executeInternal(RollbackTaskCompletionCommand cmd) {
        userStatisticsService.rollbackProgress(cmd.userId(), StatisticTypeEnum.COMPLETED_TASKS);
        return null;
    }
}
