package edu.pjwstk.notification.dto;

import edu.pjwstk.notification.enums.NotificationType;
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
}
