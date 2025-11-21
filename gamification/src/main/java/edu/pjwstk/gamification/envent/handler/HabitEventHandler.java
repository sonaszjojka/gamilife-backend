package edu.pjwstk.gamification.envent.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.HabitStreakDownEvent;
import edu.pjwstk.core.event.HabitStreakUpEvent;
import edu.pjwstk.gamification.service.UserStatisticsService;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
@AllArgsConstructor
@Slf4j
public class HabitEventHandler {

    private final UserStatisticsService userStatisticsService;

    @Async("gamificationEventExecutor")
    @EventListener
    @Retryable
    public void onHabitStreakUp(HabitStreakUpEvent event) {
        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.HABIT_STREAK);
    }

    @Async("gamificationEventExecutor")
    @EventListener
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
