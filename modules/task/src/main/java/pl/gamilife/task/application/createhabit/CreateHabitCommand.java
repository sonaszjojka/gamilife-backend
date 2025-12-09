package pl.gamilife.task.application.createhabit;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.Duration;
import java.util.UUID;

public record CreateHabitCommand(
        @NotNull
        UUID taskId,

        @NotNull(message = "Cycle length cannot be null")
        Duration cycleLength
) implements Command {
    @Override
    public void validate() {

    }
}
