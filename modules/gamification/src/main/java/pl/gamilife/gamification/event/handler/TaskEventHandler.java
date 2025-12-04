package pl.gamilife.gamification.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.gamification.usecase.processtaskcompletion.ProcessTaskCompletionCommand;
import pl.gamilife.gamification.usecase.processtaskcompletion.ProcessTaskCompletionUseCase;
import pl.gamilife.gamification.usecase.rollbacktaskcompletion.RollbackTaskCompletionCommand;
import pl.gamilife.gamification.usecase.rollbacktaskcompletion.RollbackTaskCompletionUseCase;
import pl.gamilife.infrastructure.core.event.TaskCompletedEvent;
import pl.gamilife.infrastructure.core.event.TaskUndoneEvent;

@Component
@AllArgsConstructor
@Slf4j
public class TaskEventHandler {

    private final ProcessTaskCompletionUseCase processTaskCompletionUseCase;
    private final RollbackTaskCompletionUseCase rollbackTaskCompletionUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onTaskCompleted(TaskCompletedEvent event) {
        processTaskCompletionUseCase.execute(new ProcessTaskCompletionCommand(
                event.getUserId(),
                event.isRewardGranted()
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onTaskUndone(TaskUndoneEvent event) {
        rollbackTaskCompletionUseCase.execute(new RollbackTaskCompletionCommand(event.getUserId()));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
