package edu.pjwstk.gamification.envent.handler;

import edu.pjwstk.core.enums.StatisticTypeEnum;
import edu.pjwstk.core.event.GroupTaskCompletedEvent;
import edu.pjwstk.core.event.GroupTaskUndoneEvent;
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
public class GroupTaskEventHandler {

    private final UserStatisticsService userStatisticsService;

    @Async("gamificationEventExecutor")
    @EventListener
    @Retryable
    public void onGroupTaskCompleted(GroupTaskCompletedEvent event) {
        userStatisticsService.registerProgress(event.getUserId(), StatisticTypeEnum.GROUP_TASKS_COMPLETED);
    }

    @Async("gamificationEventExecutor")
    @EventListener
    @Retryable
    public void onGroupTaskUndone(GroupTaskUndoneEvent event) {
        userStatisticsService.rollbackProgress(event.getUserId(), StatisticTypeEnum.GROUP_TASKS_COMPLETED);
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
