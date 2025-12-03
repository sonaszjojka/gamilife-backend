package pl.gamilife.communication.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.communication.dto.NotificationDto;
import pl.gamilife.communication.enums.NotificationType;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationCommand;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationUseCase;
import pl.gamilife.infrastructure.core.event.GroupInvitationCreatedEvent;

import java.util.Map;

@Slf4j
@Component
@AllArgsConstructor
public class NotificationEventHandler {

    private final SendUserNotificationUseCase sendUserNotificationUseCase;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupInvitationCreated(GroupInvitationCreatedEvent event) {
        NotificationDto notificationDto = NotificationDto.builder()
                .notificationType(NotificationType.GROUP_INVITATION)
                .title("New Group Invitation")
                .message("You have a new group invitation")
                .data(Map.of("invitation-link", event.getInvitationLink()))
                .build();

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.getUserId(),
                notificationDto
        ));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
