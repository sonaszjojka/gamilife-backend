package pl.gamilife.gamification.usecase.processitemacquisition;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.service.UserStatisticsService;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

@Service
@AllArgsConstructor
public class ProcessItemAcquisitionUseCaseImpl implements ProcessItemAcquisitionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(ProcessItemAcquisitionCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.OWNED_ITEMS);
        return null;
    }
}
