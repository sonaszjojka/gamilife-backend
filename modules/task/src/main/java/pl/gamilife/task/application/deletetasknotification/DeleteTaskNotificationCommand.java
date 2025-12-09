package pl.gamilife.task.application.deletetasknotification;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteTaskNotificationCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID taskId,

        @NotNull
        UUID taskNotificationId
) implements Command {
    @Override
    public void validate() {

    }
}
