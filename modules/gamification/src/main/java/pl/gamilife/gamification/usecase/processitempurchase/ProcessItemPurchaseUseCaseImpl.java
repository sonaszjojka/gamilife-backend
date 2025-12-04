package pl.gamilife.gamification.usecase.processitempurchase;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.service.UserStatisticsService;
import pl.gamilife.infrastructure.core.enums.StatisticTypeEnum;

import java.util.Set;

@Service
@AllArgsConstructor
public class ProcessItemPurchaseUseCaseImpl implements ProcessItemPurchaseUseCase {

    private final UserStatisticsService userStatisticsService;

    @Override
    public Void execute(ProcessItemPurchaseCommand cmd) {
        userStatisticsService.registerProgressForAll(
                cmd.userId(),
                Set.of(StatisticTypeEnum.ITEMS_PURCHASED, StatisticTypeEnum.OWNED_ITEMS)
        );

        return null;
    }
}
