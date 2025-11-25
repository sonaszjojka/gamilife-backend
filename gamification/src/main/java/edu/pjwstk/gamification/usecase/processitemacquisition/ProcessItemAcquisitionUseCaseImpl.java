package edu.pjwstk.gamification.usecase.processitemacquisition;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class ProcessItemAcquisitionUseCaseImpl implements ProcessItemAcquisitionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void executeInternal(ProcessItemAcquisitionCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.OWNED_ITEMS);
        return null;
    }
}
