package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.PomodoroTaskCompletedEvent;
import edu.pjwstk.core.event.PomodoroTaskUndoneEvent;
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
public class PomodoroEventHandler {

    private final UserStatisticsService userStatisticsService;
    private final RewardService rewardService;

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onPomodoroTaskCompleted(PomodoroTaskCompletedEvent event) {
        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.POMODORO_TASKS_COMPLETED);

        if (!event.isRewardGranted()) {
            rewardService.rewardUser(
                    event.getUserId(),
                    StatisticTypeEnum.POMODORO_TASKS_COMPLETED
            );
        }
    }

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onPomodoroTaskUndone(PomodoroTaskUndoneEvent event) {
        userStatisticsService.rollbackProgress(event.getUserId(), StatisticTypeEnum.POMODORO_TASKS_COMPLETED);
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
