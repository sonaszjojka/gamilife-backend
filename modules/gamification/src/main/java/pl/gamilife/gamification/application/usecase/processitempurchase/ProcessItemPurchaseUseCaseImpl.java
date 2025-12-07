package pl.gamilife.gamification.application.usecase.processitempurchase;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.gamification.domain.model.enums.StatisticTypeEnum;
import pl.gamilife.gamification.domain.service.UserStatisticsService;

import java.util.Set;

@Service
@Transactional
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
