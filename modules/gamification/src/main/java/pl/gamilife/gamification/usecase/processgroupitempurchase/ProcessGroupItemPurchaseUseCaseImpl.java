package pl.gamilife.gamification.usecase.processgroupitempurchase;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.service.UserStatisticsService;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

@Service
@AllArgsConstructor
public class ProcessGroupItemPurchaseUseCaseImpl implements ProcessGroupItemPurchaseUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(ProcessGroupItemPurchaseCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.GROUP_ITEMS_PURCHASED);
        return null;
    }
}
