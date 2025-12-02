package pl.gamilife.gamification.usecase.processitemacquisition;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import pl.gamilife.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
