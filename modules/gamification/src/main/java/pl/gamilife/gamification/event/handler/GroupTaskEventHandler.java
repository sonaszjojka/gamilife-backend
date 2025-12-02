package pl.gamilife.gamification.event.handler;

import pl.gamilife.infrastructure.core.event.GroupTaskCompletedEvent;
import pl.gamilife.infrastructure.core.event.GroupTaskUndoneEvent;
import pl.gamilife.gamification.usecase.processgrouptaskcompletion.ProcessGroupTaskCompletionCommand;
import pl.gamilife.gamification.usecase.processgrouptaskcompletion.ProcessGroupTaskCompletionUseCase;
import pl.gamilife.gamification.usecase.rollbackgrouptaskcompletion.RollbackGroupTaskCompletionCommand;
import pl.gamilife.gamification.usecase.rollbackgrouptaskcompletion.RollbackGroupTaskCompletionUseCase;
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
public class GroupTaskEventHandler {

    private final ProcessGroupTaskCompletionUseCase processGroupTaskCompletionUseCase;
    private final RollbackGroupTaskCompletionUseCase rollbackGroupTaskCompletionUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupTaskCompleted(GroupTaskCompletedEvent event) {
        processGroupTaskCompletionUseCase.execute(new ProcessGroupTaskCompletionCommand(
                event.getUserId(),
                event.isRewardGranted()
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupTaskUndone(GroupTaskUndoneEvent event) {
        rollbackGroupTaskCompletionUseCase.execute(new RollbackGroupTaskCompletionCommand(event.getUserId()));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
