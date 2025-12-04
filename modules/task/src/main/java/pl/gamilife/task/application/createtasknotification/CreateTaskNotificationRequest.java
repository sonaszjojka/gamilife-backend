package pl.gamilife.task.application.createtasknotification;

import java.time.LocalDateTime;

public record CreateTaskNotificationRequest(
        LocalDateTime sendDate
) {
}
