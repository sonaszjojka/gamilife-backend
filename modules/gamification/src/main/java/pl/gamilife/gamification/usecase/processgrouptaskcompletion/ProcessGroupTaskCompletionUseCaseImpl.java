package pl.gamilife.gamification.usecase.processgrouptaskcompletion;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import pl.gamilife.gamification.service.RewardService;
import pl.gamilife.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@AllArgsConstructor
public class ProcessGroupTaskCompletionUseCaseImpl implements ProcessGroupTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    public Void execute(ProcessGroupTaskCompletionCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.GROUP_TASKS_COMPLETED);

        if (!cmd.rewardGranted()) {
            rewardService.rewardUser(
                    cmd.userId(),
                    StatisticTypeEnum.GROUP_TASKS_COMPLETED
            );
        }

        return null;
    }
}
