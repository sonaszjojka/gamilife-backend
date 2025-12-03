package pl.gamilife.gamification.usecase.processtaskcompletion;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.service.RewardService;
import pl.gamilife.gamification.service.UserStatisticsService;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

@Service
@Transactional
@AllArgsConstructor
public class ProcessTaskCompletionUseCaseImpl implements ProcessTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    public Void execute(ProcessTaskCompletionCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.COMPLETED_TASKS);

        if (!cmd.rewardGranted()) {
            rewardService.rewardUser(
                    cmd.userId(),
                    StatisticTypeEnum.COMPLETED_TASKS
            );
        }

        return null;
    }
}
