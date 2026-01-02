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
import pl.gamilife.communication.usecase.bulksendnotification.BulkSendNotificationCommand;
import pl.gamilife.communication.usecase.bulksendnotification.BulkSendNotificationUseCase;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationCommand;
import pl.gamilife.communication.usecase.sendusernotification.SendUserNotificationUseCase;
import pl.gamilife.shared.kernel.event.*;

import java.util.Map;

@Slf4j
@Component
@AllArgsConstructor
public class NotificationEventHandler {

    private final SendUserNotificationUseCase sendUserNotificationUseCase;
    private final BulkSendNotificationUseCase bulkSendNotificationUseCase;
    private final UserApi userApi;

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupInvitationCreated(GroupInvitationCreatedEvent event) {
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.GROUP_INVITATION,
                Map.of(
                        "invitationLink", event.invitationLink(),
                        "groupName", event.groupName()
                )
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.userId(),
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
                Map.of(
                        "groupName", event.groupName(),
                        "accepted", event.accepted(),
                        "groupId", event.groupId()
                )
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

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onJoinedGroupEvent(JoinedGroupEvent event) {
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.NEW_GROUP_MEMBER,
                Map.of(
                        "groupName", event.groupName(),
                        "username", event.username(),
                        "groupId", event.groupId()
                )
        );

        bulkSendNotificationUseCase.execute(new BulkSendNotificationCommand(
                event.activeMembersUserIds(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGroupMemberLeft(GroupMemberLeftEvent event) {
        BasicUserInfoDto basicUserInfoDto = userApi.getUserById(event.userId()).orElseThrow(
                () -> new IllegalStateException("GroupRequestCreatedEvent refers to a non-existent user")
        );

        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.GROUP_MEMBER_LEFT,
                Map.of(
                        "groupName", event.groupName(),
                        "username", basicUserInfoDto.username(),
                        "groupId", event.groupId()
                )
        );

        bulkSendNotificationUseCase.execute(new BulkSendNotificationCommand(
                event.activeMembersUserIds(),
                notificationDto
        ));
    }

    @Async("eventExecutor")
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    @Retryable
    public void onGamificationValuesChanged(GamificationValuesChangedEvent event) {
        NotificationDto notificationDto = NotificationDto.create(
                NotificationType.GAMIFICATION_VALUES_CHANGED,
                Map.of(
                        "userId", event.userId(),
                        "username", event.username(),
                        "level", event.level(),
                        "experience", event.experience(),
                        "money", event.money(),
                        "requiredExperienceForNextLevel", event.requiredExperienceForNextLevel()
                )
        );

        sendUserNotificationUseCase.execute(new SendUserNotificationCommand(
                event.userId(),
                notificationDto
        ));
    }

    @Recover
    public void onMultipleFailure(Exception ex, Object event) {
        log.error("Could not process event: {}", event);
        log.error("Exception: {}", ex.getMessage(), ex);
    }
}
