package pl.gamilife.task.infrastructure.web.request;

import jakarta.validation.constraints.Future;

import java.time.Instant;

public record EditTaskNotificationRequest(
        @Future
        Instant sendDate
) {
}
