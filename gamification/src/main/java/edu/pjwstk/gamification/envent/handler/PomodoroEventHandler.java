package edu.pjwstk.gamification.envent.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.PomodoroTaskCompletedEvent;
import edu.pjwstk.core.event.PomodoroTaskUndoneEvent;
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
public class PomodoroEventHandler {

    private final UserStatisticsService userStatisticsService;

    @Async("gamificationEventExecutor")
    @EventListener
    @Retryable
    public void onPomodoroTaskCompleted(PomodoroTaskCompletedEvent event) {
        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.POMODORO_TASKS_COMPLETED);
    }

    @Async("gamificationEventExecutor")
    @EventListener
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
