package edu.pjwstk.tasks.edittasknotification;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.UUID;

public record EditTaskNotificationResponse(
        Integer id,
        LocalDateTime sendDate,
        UUID taskId
) {
}
