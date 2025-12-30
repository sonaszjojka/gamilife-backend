package pl.gamilife.communication.event.handler;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.communication.dto.NotificationDto;
import pl.gamilife.communication.enums.NotificationType;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationCommand;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationUseCase;
import pl.gamilife.shared.kernel.event.GroupInvitationCreatedEvent;
import pl.gamilife.shared.kernel.event.GroupItemUsedEvent;

import java.util.Map;

@Slf4j
@Component
@AllArgsConstructor
public class NotificationEventHandler {

    private final SendUserNotificationUseCase sendUserNotificationUseCase;
    private final UserApi userApi;

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

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupItemUsed(GroupItemUsedEvent event) {
        BasicUserInfoDto basicUserInfoDto = userApi.getUserById(event.userId()).get();
        NotificationDto notificationDto = NotificationDto.builder()
                .notificationType(NotificationType.GROUP_ITEM_USED)
                .title("Group Item Used")
                .message(String.format("%s used %s in your group!", basicUserInfoDto.username(), event.groupItemName()))
                .data(Map.of("item-name", event.groupItemName()))
                .build();

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.adminId(),
                notificationDto
        ));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
