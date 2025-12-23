package pl.gamilife.task.application.createtasknotification;

import java.time.Instant;

public record CreateTaskNotificationRequest(
        Instant sendDate
) {
}
