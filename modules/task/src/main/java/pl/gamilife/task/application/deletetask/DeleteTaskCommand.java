package pl.gamilife.task.application.deletetask;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteTaskCommand(
        @NotNull
        UUID userId,

        @NotNull
        UUID taskId
) implements Command {
}
