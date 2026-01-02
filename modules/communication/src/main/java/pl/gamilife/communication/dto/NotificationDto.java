package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.NotificationType;
import pl.gamilife.communication.model.NotificationRetry;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

public record NotificationDto(
        UUID id,
        NotificationType notificationType,
        Instant timestamp,
        Map<String, Object> data
) {

    public static NotificationDto from(NotificationRetry notificationRetry) {
        return new NotificationDto(
                UUID.randomUUID(),
                NotificationType.fromId(notificationRetry.getNotificationTypeId()),
                notificationRetry.getOriginalTimestamp(),
                notificationRetry.getData()
        );
    }

    public static NotificationDto create(NotificationType notificationType, Map<String, Object> data) {
        return new NotificationDto(
                UUID.randomUUID(),
                notificationType,
                Instant.now(),
                data
        );
    }
}
