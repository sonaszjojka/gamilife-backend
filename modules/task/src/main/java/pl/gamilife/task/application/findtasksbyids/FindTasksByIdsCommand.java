package pl.gamilife.task.application.findtasksbyids;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.Collection;
import java.util.UUID;

public record FindTasksByIdsCommand(@NotNull Collection<UUID> taskIds) implements Command {
}
