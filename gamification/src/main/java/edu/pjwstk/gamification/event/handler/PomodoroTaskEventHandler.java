package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.event.PomodoroTaskCompletedEvent;
import edu.pjwstk.core.event.PomodoroTaskUndoneEvent;
import edu.pjwstk.gamification.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionCommand;
import edu.pjwstk.gamification.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionUseCase;
import edu.pjwstk.gamification.usecase.rollbackpomodorotaskcompletion.RollbackPomodoroTaskCompletionCommand;
import edu.pjwstk.gamification.usecase.rollbackpomodorotaskcompletion.RollbackPomodoroTaskCompletionUseCase;
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
public class PomodoroTaskEventHandler {

    private final ProcessPomodoroTaskCompletionUseCase processPomodoroTaskCompletionUseCase;
    private final RollbackPomodoroTaskCompletionUseCase rollbackPomodoroTaskCompletionUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onPomodoroTaskCompleted(PomodoroTaskCompletedEvent event) {
        processPomodoroTaskCompletionUseCase.execute(new ProcessPomodoroTaskCompletionCommand(
                event.getUserId(),
                event.isRewardGranted()
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onPomodoroTaskUndone(PomodoroTaskUndoneEvent event) {
        rollbackPomodoroTaskCompletionUseCase.execute(new RollbackPomodoroTaskCompletionCommand(event.getUserId()));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
