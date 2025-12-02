package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.NotificationType;
import pl.gamilife.communication.model.NotificationRetry;
import lombok.Builder;
import lombok.Value;

import java.time.Instant;
import java.util.Map;
import java.util.UUID;

@Builder
@Value
public class NotificationDto {
    @Builder.Default
    UUID id = UUID.randomUUID();
    NotificationType notificationType;
    String title;
    String message;
    @Builder.Default
    Instant timestamp = Instant.now();
    Map<String, Object> data;

    public static NotificationDto from(NotificationRetry notificationRetry) {
        return NotificationDto.builder()
                .id(notificationRetry.getId())
                .title(notificationRetry.getTitle())
                .message(notificationRetry.getMessage())
                .timestamp(notificationRetry.getOriginalTimestamp())
                .data(notificationRetry.getData())
                .notificationType(NotificationType.fromId(notificationRetry.getNotificationTypeId()))
                .build();
    }
}
