package pl.gamilife.task.controllers.response;

import java.time.Instant;
import java.util.UUID;

public record EditTaskNotificationResponse(
        UUID id,
        Instant sendDate,
        UUID taskId
) {
}
