package pl.gamilife.communication.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.communication.dto.EmailVerificationEmailParameters;
import pl.gamilife.communication.dto.ForgotPasswordEmailParameters;
import pl.gamilife.communication.dto.GroupInvitationEmailParameters;
import pl.gamilife.communication.usecase.senduseremail.SendUserEmailCommand;
import pl.gamilife.communication.usecase.senduseremail.SendUserEmailUseCase;
import pl.gamilife.shared.kernel.event.EmailVerificationRequestedEvent;
import pl.gamilife.shared.kernel.event.ForgotPasswordCodeRequestedEvent;
import pl.gamilife.shared.kernel.event.GroupInvitationCreatedEvent;

@Slf4j
@Component
@AllArgsConstructor
public class EmailEventHandler {

    private final SendUserEmailUseCase sendUserEmailUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onEmailVerificationRequested(EmailVerificationRequestedEvent event) {
        log.info("Email verification requested for user {}", event.userId());
        sendUserEmailUseCase.execute(new SendUserEmailCommand(
                event.userId(),
                new EmailVerificationEmailParameters(event.verificationCode())
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupInvitationCreated(GroupInvitationCreatedEvent event) {
        sendUserEmailUseCase.execute(new SendUserEmailCommand(
                event.userId(),
                new GroupInvitationEmailParameters(
                        event.groupId(),
                        event.groupName(),
                        event.groupInvitationId(),
                        event.token()
                )
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onPasswordResetRequested(ForgotPasswordCodeRequestedEvent event) {
        sendUserEmailUseCase.execute(new SendUserEmailCommand(
                event.userId(),
                new ForgotPasswordEmailParameters(event.forgotPasswordCode())
        ));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
