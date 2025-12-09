package pl.gamilife.task.application.deletehabit;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record DeleteHabitCommand(@NotNull UUID taskId) implements Command {
    @Override
    public void validate() {

    }
}
