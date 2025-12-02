package edu.pjwstk.gamification.usecase.processtaskcompletion;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
