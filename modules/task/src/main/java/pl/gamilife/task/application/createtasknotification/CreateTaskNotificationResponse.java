package pl.gamilife.task.application.createtasknotification;

import java.time.LocalDateTime;
import java.util.UUID;

public record CreateTaskNotificationResponse(
        Integer id,
        LocalDateTime sendDate,
        UUID taskId
) {
}
