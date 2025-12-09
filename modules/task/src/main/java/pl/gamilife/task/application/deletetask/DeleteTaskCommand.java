package pl.gamilife.task.application.deletetask;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteTaskCommand(
        UUID userId,
        UUID taskId
) implements Command {
    @Override
    public void validate() {

    }
}
