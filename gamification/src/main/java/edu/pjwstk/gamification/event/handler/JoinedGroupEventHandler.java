package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.JoinedGroupEvent;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Component
@AllArgsConstructor
@Slf4j
public class JoinedGroupEventHandler {

    private final UserStatisticsService userStatisticsService;

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupJoined(JoinedGroupEvent event) {
        if (!event.isFirstTimeJoin()) {
            return;
        }

        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.JOINED_GROUPS);
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
