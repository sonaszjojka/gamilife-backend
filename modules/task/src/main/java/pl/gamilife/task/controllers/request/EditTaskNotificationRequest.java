package pl.gamilife.task.controllers.request;

import java.time.Instant;

public record EditTaskNotificationRequest(
        Instant sendDate
) {
}
