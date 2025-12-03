package pl.gamilife.gamification.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.gamification.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionCommand;
import pl.gamilife.gamification.usecase.processpomodorotaskcompletion.ProcessPomodoroTaskCompletionUseCase;
import pl.gamilife.gamification.usecase.rollbackpomodorotaskcompletion.RollbackPomodoroTaskCompletionCommand;
import pl.gamilife.gamification.usecase.rollbackpomodorotaskcompletion.RollbackPomodoroTaskCompletionUseCase;
import pl.gamilife.infrastructure.core.event.PomodoroTaskCompletedEvent;
import pl.gamilife.infrastructure.core.event.PomodoroTaskUndoneEvent;

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
