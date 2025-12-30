package pl.gamilife.gamification.application.usecase.processitemacquisition;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

@Service
@Transactional
@AllArgsConstructor
public class ProcessItemAcquisitionUseCaseImpl implements ProcessItemAcquisitionUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(ProcessItemAcquisitionCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.OWNED_ITEMS, cmd.progressAmount());
        return null;
    }
}
