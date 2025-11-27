package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.event.TaskCompletedEvent;
import edu.pjwstk.core.event.TaskUndoneEvent;
import edu.pjwstk.gamification.usecase.processtaskcompletion.ProcessTaskCompletionCommand;
import edu.pjwstk.gamification.usecase.processtaskcompletion.ProcessTaskCompletionUseCase;
import edu.pjwstk.gamification.usecase.rollbacktaskcompletion.RollbackTaskCompletionCommand;
import edu.pjwstk.gamification.usecase.rollbacktaskcompletion.RollbackTaskCompletionUseCase;
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

    private final ProcessTaskCompletionUseCase processTaskCompletionUseCase;
    private final RollbackTaskCompletionUseCase rollbackTaskCompletionUseCase;

    @Async("gamificationEventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onTaskCompleted(TaskCompletedEvent event) {
        processTaskCompletionUseCase.execute(new ProcessTaskCompletionCommand(
                event.getUserId(),
                event.isRewardGranted()
        ));
    }

    @Async("gamificationEventExecutor")
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
