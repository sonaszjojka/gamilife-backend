package pl.gamilife.gamification.event.handler;

import pl.gamilife.infrastructure.core.event.JoinedGroupEvent;
import pl.gamilife.gamification.usecase.processgroupjoin.ProcessGroupJoinCommand;
import pl.gamilife.gamification.usecase.processgroupjoin.ProcessGroupJoinUseCase;
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
public class JoinedGroupEventHandler {

    private final ProcessGroupJoinUseCase processGroupJoinUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupJoined(JoinedGroupEvent event) {
        processGroupJoinUseCase.execute(new ProcessGroupJoinCommand(event.getUserId(), event.isFirstTimeJoin()));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
