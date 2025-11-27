package edu.pjwstk.gamification.usecase.processgroupjoin;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class ProcessGroupJoinUseCaseImpl implements ProcessGroupJoinUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void executeInternal(ProcessGroupJoinCommand cmd) {
        if (cmd.isFirstTimeJoin()) {
            userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.JOINED_GROUPS);
        }

        return null;
    }
}
