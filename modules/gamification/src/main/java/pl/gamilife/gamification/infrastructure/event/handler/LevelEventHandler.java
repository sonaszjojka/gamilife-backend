package pl.gamilife.gamification.infrastructure.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.gamification.application.usecase.processonboardingcompletion.ProcessOnboardingCompletionCommand;
import pl.gamilife.gamification.application.usecase.processonboardingcompletion.ProcessOnboardingCompletionUseCase;
import pl.gamilife.gamification.application.usecase.processuserregistered.ProcessUserRegisteredCommand;
import pl.gamilife.gamification.application.usecase.processuserregistered.ProcessUserRegisteredUseCase;
import pl.gamilife.shared.kernel.event.OnboardingCompletedEvent;
import pl.gamilife.shared.kernel.event.UserRegisteredEvent;

@Component
@Slf4j
@AllArgsConstructor
public class LevelEventHandler {

    private final ProcessUserRegisteredUseCase processUserRegisteredUseCase;
    private final ProcessOnboardingCompletionUseCase processOnboardingCompletionUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onUserRegistered(UserRegisteredEvent event) {
        processUserRegisteredUseCase.execute(new ProcessUserRegisteredCommand(event.userId()));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onOnboardingCompleted(OnboardingCompletedEvent event) {
        processOnboardingCompletionUseCase.execute(new ProcessOnboardingCompletionCommand(event.userId()));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }

}
