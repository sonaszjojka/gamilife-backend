package pl.gamilife.gamification.infrastructure.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.gamification.application.usecase.processitemacquisition.ProcessItemAcquisitionCommand;
import pl.gamilife.gamification.application.usecase.processitemacquisition.ProcessItemAcquisitionUseCase;
import pl.gamilife.gamification.application.usecase.processitempurchase.ProcessItemPurchaseCommand;
import pl.gamilife.gamification.application.usecase.processitempurchase.ProcessItemPurchaseUseCase;
import pl.gamilife.shared.kernel.event.ItemAcquiredEvent;
import pl.gamilife.shared.kernel.event.ItemBoughtEvent;

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
        processItemPurchaseUseCase.execute(new ProcessItemPurchaseCommand(event.getUserId(), event.getAmount()));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onItemAcquired(ItemAcquiredEvent event) {
        processItemAcquisitionUseCase.execute(new ProcessItemAcquisitionCommand(
                event.userId(),
                event.itemNames().size()
        ));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
