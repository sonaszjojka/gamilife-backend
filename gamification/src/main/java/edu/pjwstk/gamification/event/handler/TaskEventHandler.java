package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.TaskCompletedEvent;
import edu.pjwstk.core.event.TaskUndoneEvent;
import edu.pjwstk.gamification.service.RewardService;
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
public class TaskEventHandler {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onTaskCompleted(TaskCompletedEvent event) {
        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.COMPLETED_TASKS);

        if (!event.isRewardGranted()) {
            rewardService.rewardUser(
                    event.getUserId(),
                    StatisticTypeEnum.COMPLETED_TASKS
            );
        }
    }

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onTaskUndone(TaskUndoneEvent event) {
        userStatisticsService.rollbackProgress(event.getUserId(), StatisticTypeEnum.COMPLETED_TASKS);
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
