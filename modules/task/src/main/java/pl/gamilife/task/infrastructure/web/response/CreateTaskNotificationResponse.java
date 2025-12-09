package pl.gamilife.task.infrastructure.web.response;

import java.time.Instant;
import java.util.UUID;

public record CreateTaskNotificationResponse(
        UUID id,
        Instant sendDate,
        UUID taskId
) {
}
