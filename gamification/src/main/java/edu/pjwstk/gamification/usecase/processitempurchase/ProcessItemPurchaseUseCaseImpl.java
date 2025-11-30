package edu.pjwstk.gamification.usecase.processitempurchase;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
