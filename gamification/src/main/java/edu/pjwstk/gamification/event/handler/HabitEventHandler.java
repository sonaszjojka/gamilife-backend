package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.HabitStreakDownEvent;
import edu.pjwstk.core.event.HabitStreakUpEvent;
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
public class HabitEventHandler {

    private final UserStatisticsService userStatisticsService;

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onHabitStreakUp(HabitStreakUpEvent event) {
        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.HABIT_STREAK);
    }

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onHabitStreakDown(HabitStreakDownEvent event) {
        userStatisticsService.rollbackProgress(event.getUserId(), StatisticTypeEnum.HABIT_STREAK);
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
