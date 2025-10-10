package edu.pjwstk.tasks.createtasknotification;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.UUID;

public record CreateTaskNotificationResponse(
        Integer id,
        LocalDateTime sendDate,
        UUID taskId
) {
}
