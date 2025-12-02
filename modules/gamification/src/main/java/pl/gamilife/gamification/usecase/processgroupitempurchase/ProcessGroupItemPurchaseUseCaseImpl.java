package pl.gamilife.gamification.usecase.processgroupitempurchase;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import pl.gamilife.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
