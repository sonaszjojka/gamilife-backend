package edu.pjwstk.gamification.usecase.rollbackgrouptaskcompletion;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
