package pl.gamilife.task.application.createtasknotification;

import jakarta.validation.constraints.Future;
import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.Instant;
import java.util.UUID;

public record CreateTaskNotificationCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID taskId,

        @Future
        @NotNull
        Instant sendDate
) implements Command {
}
