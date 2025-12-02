package edu.pjwstk.gamification.usecase.processgroupitempurchase;

import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.UserStatisticsService;
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
