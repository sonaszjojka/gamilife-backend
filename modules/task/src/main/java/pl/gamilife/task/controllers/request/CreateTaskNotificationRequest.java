package pl.gamilife.task.controllers.request;

import java.time.Instant;

public record CreateTaskNotificationRequest(
        Instant sendDate
) {
}
