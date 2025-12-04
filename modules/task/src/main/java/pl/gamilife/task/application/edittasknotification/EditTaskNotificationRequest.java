package pl.gamilife.task.application.edittasknotification;

import java.time.LocalDateTime;

public record EditTaskNotificationRequest(
        LocalDateTime sendDate
) {
}
