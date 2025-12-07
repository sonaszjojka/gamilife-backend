package pl.gamilife.gamification.application.usecase.processgroupjoin;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

@Service
@Transactional
@AllArgsConstructor
public class ProcessGroupJoinUseCaseImpl implements ProcessGroupJoinUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(ProcessGroupJoinCommand cmd) {
        if (cmd.isFirstTimeJoin()) {
            userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.JOINED_GROUPS);
        }

        return null;
    }
}
