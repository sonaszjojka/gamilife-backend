package pl.gamilife.task.application.edittasknotification;

import java.time.LocalDateTime;
import java.util.UUID;

public record EditTaskNotificationResponse(
        Integer id,
        LocalDateTime sendDate,
        UUID taskId
) {
}
