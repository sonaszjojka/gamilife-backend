package pl.gamilife.task.infrastructure.web.request;

import jakarta.validation.constraints.Future;
import jakarta.validation.constraints.NotNull;

import java.time.Instant;

public record EditTaskNotificationRequest(
        @NotNull
        @Future
        Instant sendDate
) {
}
