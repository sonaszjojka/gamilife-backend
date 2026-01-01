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
import pl.gamilife.shared.kernel.event.*;

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
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.GROUP_INVITATION,
                Map.of("invitationLink", event.getInvitationLink())
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.getUserId(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupItemUsed(GroupItemUsedEvent event) {
        BasicUserInfoDto basicUserInfoDto = userApi.getUserById(event.userId()).orElseThrow(
                () -> new IllegalStateException("GroupItemUsedEvent refers to a non-existent user")
        );
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.GROUP_ITEM_USED,
                Map.of("itemName", event.groupItemName(), "username", basicUserInfoDto.username())
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.adminId(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onItemAcquired(ItemAcquiredEvent event) {
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.ITEM_ACQUIRED,
                Map.of("itemNames", event.itemNames())
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.userId(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onLevelUp(LevelUpEvent event) {
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.LEVEL_UP,
                Map.of("level", event.level())
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.userId(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onAchievementUnlocked(AchievementUnlockedEvent event) {
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.ACHIEVEMENT_UNLOCKED,
                Map.of("achievementName", event.achievementName())
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.userId(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupRequestStatusChanged(GroupRequestStatusChangedEvent event) {
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.GROUP_REQUEST_STATUS_UPDATED,
                Map.of("groupName", event.groupName(), "accepted", event.accepted())
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.requesterUserId(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupRequestCreated(GroupRequestCreatedEvent event) {
        BasicUserInfoDto basicUserInfoDto = userApi.getUserById(event.requesterUserId()).orElseThrow(
                () -> new IllegalStateException("GroupRequestCreatedEvent refers to a non-existent user")
        );

        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.NEW_GROUP_REQUEST,
                Map.of(
                        "groupName", event.groupName(),
                        "username", basicUserInfoDto.username(),
                        "groupId", event.groupId()
                )
        );

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
