package pl.gamilife.task.application.createtasknotification;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.UUID;

public record CreateTaskNotificationResponse(
        UUID id,
        Instant sendDate,
        UUID taskId
) {
}
