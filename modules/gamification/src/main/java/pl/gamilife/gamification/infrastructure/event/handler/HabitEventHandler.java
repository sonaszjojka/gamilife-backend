package pl.gamilife.gamification.infrastructure.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.gamification.application.usecase.processhabitstreakchange.ProcessHabitStreakChangeCommand;
import pl.gamilife.gamification.application.usecase.processhabitstreakchange.ProcessHabitStreakChangeUseCase;
import pl.gamilife.infrastructure.core.event.HabitStreakChangedEvent;

@Component
@AllArgsConstructor
@Slf4j
public class HabitEventHandler {

    private final ProcessHabitStreakChangeUseCase processHabitStreakChangeUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onHabitStreakUp(HabitStreakChangedEvent event) {
        processHabitStreakChangeUseCase.execute(new ProcessHabitStreakChangeCommand(
                event.getUserId(),
                event.getStreakValue()
        ));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
