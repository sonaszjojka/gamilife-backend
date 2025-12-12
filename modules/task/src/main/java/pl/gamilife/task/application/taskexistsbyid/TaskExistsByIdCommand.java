package pl.gamilife.task.application.taskexistsbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record TaskExistsByIdCommand(@NotNull UUID taskId) implements Command {
}
