package pl.gamilife.task.application.findhabitbyid;

import jakarta.validation.constraints.NotNull;
import pl.gamilife.shared.kernel.architecture.Command;

import java.time.ZoneId;
import java.util.UUID;

public record FindHabitByIdCommand(
        @NotNull UUID habitId,

        @NotNull
        UUID userId,

        ZoneId zoneId) implements Command {
}
