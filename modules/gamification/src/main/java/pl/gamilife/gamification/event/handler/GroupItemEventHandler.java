package pl.gamilife.gamification.event.handler;

import pl.gamilife.infrastructure.core.event.GroupItemPurchasedEvent;
import pl.gamilife.gamification.usecase.processgroupitempurchase.ProcessGroupItemPurchaseCommand;
import pl.gamilife.gamification.usecase.processgroupitempurchase.ProcessGroupItemPurchaseUseCase;
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
public class GroupItemEventHandler {

    private final ProcessGroupItemPurchaseUseCase processGroupItemPurchaseUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupItemPurchased(GroupItemPurchasedEvent event) {
        processGroupItemPurchaseUseCase.execute(new ProcessGroupItemPurchaseCommand(event.getUserId()));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
