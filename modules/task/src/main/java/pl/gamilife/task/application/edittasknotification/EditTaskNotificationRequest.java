package pl.gamilife.task.application.edittasknotification;

import java.time.Instant;

public record EditTaskNotificationRequest(
        Instant sendDate
) {
}
