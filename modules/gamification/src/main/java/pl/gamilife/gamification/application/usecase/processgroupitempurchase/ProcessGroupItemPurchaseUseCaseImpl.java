package pl.gamilife.gamification.application.usecase.processgroupitempurchase;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

@Service
@Transactional
@AllArgsConstructor
public class ProcessGroupItemPurchaseUseCaseImpl implements ProcessGroupItemPurchaseUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(ProcessGroupItemPurchaseCommand cmd) {
        userStatisticsService.registerProgress(cmd.userId(), StatisticTypeEnum.GROUP_ITEMS_PURCHASED);
        return null;
    }
}
