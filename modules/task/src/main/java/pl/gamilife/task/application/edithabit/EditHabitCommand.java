package pl.gamilife.task.application.edithabit;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.Duration;
import java.util.UUID;

public record EditHabitCommand(
        @NotNull
        UUID taskId,

        @NotNull
        Duration cycleLength,

        Boolean finished
) implements Command {
    @Override
    public void validate() {

    }
}
