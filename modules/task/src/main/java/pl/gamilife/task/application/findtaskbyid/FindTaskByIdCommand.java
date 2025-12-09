package pl.gamilife.task.application.findtaskbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record FindTaskByIdCommand(@NotNull UUID taskId) implements Command {
    @Override
    public void validate() {

    }
}
