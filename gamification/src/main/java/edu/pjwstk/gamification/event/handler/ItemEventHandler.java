package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.ItemAcquiredEvent;
import edu.pjwstk.core.event.ItemBoughtEvent;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

import java.util.Set;

@Component
@AllArgsConstructor
@Slf4j
public class ItemEventHandler {

    private final UserStatisticsService userStatisticsService;

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onItemBought(ItemBoughtEvent event) {
        userStatisticsService.registerProgressForAll(
                event.getUserId(),
                Set.of(StatisticTypeEnum.ITEMS_PURCHASED, StatisticTypeEnum.OWNED_ITEMS)
        );
    }

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onItemAcquired(ItemAcquiredEvent event) {
        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.OWNED_ITEMS);
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
