package edu.pjwstk.communication.event.handler;

import edu.pjwstk.communication.dto.EmailVerificationEmailParameters;
import edu.pjwstk.communication.dto.ForgotPasswordEmailParameters;
import edu.pjwstk.communication.dto.GroupInvitationEmailParameters;
import edu.pjwstk.communication.usecase.senduseremail.SendUserEmailCommand;
import edu.pjwstk.communication.usecase.senduseremail.SendUserEmailUseCase;
import edu.pjwstk.core.event.EmailVerificationRequestedEvent;
import edu.pjwstk.core.event.GroupInvitationCreatedEvent;
import edu.pjwstk.core.event.PasswordResetRequestedEvent;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Slf4j
@Component
@AllArgsConstructor
public class EmailEventHandler {

    private final SendUserEmailUseCase sendUserEmailUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onEmailVerificationRequested(EmailVerificationRequestedEvent event) {
        log.info("Email verification requested for user {}", event.getUserId());
        sendUserEmailUseCase.execute(new SendUserEmailCommand(
                event.getUserId(),
                new EmailVerificationEmailParameters(event.getVerificationLink())
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupInvitationCreated(GroupInvitationCreatedEvent event) {
        sendUserEmailUseCase.execute(new SendUserEmailCommand(
                event.getUserId(),
                new GroupInvitationEmailParameters(
                        event.getJoinCode(),
                        event.getInvitationLink()
                )
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onPasswordResetRequested(PasswordResetRequestedEvent event) {
        sendUserEmailUseCase.execute(new SendUserEmailCommand(
                event.getUserId(),
                new ForgotPasswordEmailParameters(event.getResetLink())
        ));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
