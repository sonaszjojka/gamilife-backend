package pl.gamilife.gamification.application.usecase.processgrouptaskcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.service.RewardService;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class ProcessGroupTaskCompletionUseCaseImpl implements ProcessGroupTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    public Void execute(ProcessGroupTaskCompletionCommand cmd) {
        for (UUID userId : cmd.userIds()) {
            userStatisticsService.registerProgress(userId, StatisticTypeEnum.GROUP_TASKS_COMPLETED);

            if (!cmd.rewardGranted()) {
                rewardService.rewardUser(
                        userId,
                        StatisticTypeEnum.GROUP_TASKS_COMPLETED
                );
            }
        }

        return null;
    }
}
