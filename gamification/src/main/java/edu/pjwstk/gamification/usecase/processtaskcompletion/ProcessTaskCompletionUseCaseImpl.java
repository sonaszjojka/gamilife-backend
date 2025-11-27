package edu.pjwstk.gamification.usecase.processtaskcompletion;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class ProcessTaskCompletionUseCaseImpl implements ProcessTaskCompletionUseCase {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Override
    @Transactional
    public Void executeInternal(ProcessTaskCompletionCommand cmd) {
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
