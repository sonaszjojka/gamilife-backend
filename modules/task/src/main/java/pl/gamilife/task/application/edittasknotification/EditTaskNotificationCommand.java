package pl.gamilife.task.application.edittasknotification;

import jakarta.validation.constraints.Future;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.Instant;
import java.util.UUID;

public record EditTaskNotificationCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID taskId,

        @NotNull
        UUID taskNotificationId,

        @NotNull
        @Future
        Instant sendDate
) implements Command {
}
