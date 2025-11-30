package edu.pjwstk.gamification.event.handler;

import edu.pjwstk.core.event.ItemAcquiredEvent;
import edu.pjwstk.core.event.ItemBoughtEvent;
import edu.pjwstk.gamification.usecase.processitemacquisition.ProcessItemAcquisitionCommand;
import edu.pjwstk.gamification.usecase.processitemacquisition.ProcessItemAcquisitionUseCase;
import edu.pjwstk.gamification.usecase.processitempurchase.ProcessItemPurchaseCommand;
import edu.pjwstk.gamification.usecase.processitempurchase.ProcessItemPurchaseUseCase;
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
public class ItemEventHandler {

    private final ProcessItemAcquisitionUseCase processItemAcquisitionUseCase;
    private final ProcessItemPurchaseUseCase processItemPurchaseUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onItemBought(ItemBoughtEvent event) {
        processItemPurchaseUseCase.execute(new ProcessItemPurchaseCommand(event.getUserId()));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onItemAcquired(ItemAcquiredEvent event) {
        processItemAcquisitionUseCase.execute(new ProcessItemAcquisitionCommand(event.getUserId()));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
