package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.gamification.event.RewardEarnedEvent;
import edu.pjwstk.gamification.service.RewardService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class RewardEarnedEventHandler {

    private final RewardService rewardService;

    public RewardEarnedEventHandler(RewardService rewardService) {
        this.rewardService = rewardService;
    }

    @Async("gamificationEventExecutor")
    @EventListener
    @Retryable
    public void onRewardEarned(RewardEarnedEvent event) {
        rewardService.rewardUser(event.getUserId(), event.getExperience(), event.getMoney());
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }

}
