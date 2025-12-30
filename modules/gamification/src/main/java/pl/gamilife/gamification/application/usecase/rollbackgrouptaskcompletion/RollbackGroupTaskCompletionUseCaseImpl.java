package pl.gamilife.gamification.application.usecase.rollbackgrouptaskcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class RollbackGroupTaskCompletionUseCaseImpl implements RollbackGroupTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(RollbackGroupTaskCompletionCommand cmd) {
        for (UUID userId : cmd.userIds()) {
            userStatisticsService.rollbackProgress(userId, StatisticTypeEnum.GROUP_TASKS_COMPLETED);
        }

        return null;
    }
}
